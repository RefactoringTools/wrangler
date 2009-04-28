%% ===========================================================================================
%% Refactoring: Search an user-selected expression/expression sequence from the current buffer.
%%
%% Copyright (C) 2006-2008  Huiqing Li, Simon Thompson

%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.

%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% Author contact: hl@kent.ac.uk, sjt@kent.ac.uk
%% 
-module(refac_sim_expr_search).

-export([sim_expr_search/4, normalise_record_expr/4]).

-compile(export_all).

-include("../include/wrangler.hrl").
%% ================================================================================================
%% @doc Search a user-selected expression or a sequence of expressions from an Erlang source file.
%%
%% <p> This functionality allows the user to search an selected expression or a sequence of expressions
%% from the current Erlang buffer. The searching ignores variables names and literals, but it takes
%% the binding structure of variables into account. Therefore the found expressions are the same to the 
%% highlighted expression up to variable renaming and literal substitution. Layout and comments are ignored 
%% by the searching.
%% </p>
%% <p> When the selected code contains multiple, but non-continuous sequence of, expressions, the first
%% continuous sequence of expressions is taken as the user-selected expression. A continuous sequence of
%% expressions is a sequence of expressions separated by ','.
%% <p>

-spec(sim_expr_search/4::(filename(), pos(), pos(), integer()) -> {ok, [{integer(), integer(), integer(), integer()}]} | {error, string()}).    
sim_expr_search(FName, Start = {Line, Col}, End = {Line1, Col1}, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:sim_expr_search(~p, {~p,~p},{~p,~p},~p).\n", [?MODULE, FName, Line, Col, Line1, Col1, TabWidth]),
    {ok, {AnnAST, _Info}} = refac_util:parse_annotate_file(FName, true, [], TabWidth),
    case refac_util:pos_to_expr_list(FName, AnnAST, Start, End, TabWidth) of
      [E| Es] ->
	  ?wrangler_io("Selected expression:\n~p\n", [[E| Es]]);
      _ -> {error, "You have not selected an expression!"}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Search the clones of an expression from Tree.
search_one_expr(Tree, Exp) ->
    Exp1  = simplify_expr(Exp),
    BdStructExp = var_binding_structure([Exp]),
    F = fun(T, Acc) ->
		case refac_util:is_expr(T) of 
		    true -> T1 = simplify_expr(T),
			    case Exp1 == T1 of 
				true -> BdStructT = var_binding_structure([T]),
					case BdStructExp == BdStructT of 
					    true ->
						{{StartLn, StartCol}, {EndLn, EndCol}}= refac_util:get_range(T),
						Acc ++ [{StartLn, StartCol, EndLn, EndCol+1}];
					    _  -> Acc
					end;
				_ -> Acc
			    end;
		    _ -> Acc
		end
	end,
    refac_syntax_lib:fold(F, [], Tree).
    

%% search the clones of an expresion sequence from Tree.			
search_expr_seq(Tree, ExpList) ->
    AllExpList = contained_exprs(Tree, length(ExpList)),
   %% ExpList1 = lists:map(fun(T) ->simplify_expr(T) end, ExpList),
    Res =lists:flatmap(fun(EL) ->
			       get_clone(ExpList, EL) end, AllExpList),
    Res.
    
    
get_clone(List1, List2) ->    
    Len1 = length(List1),
    Len2 = length(List2),
    case Len1 =< Len2 of 
	true -> 
	    List11 = lists:map(fun(T) -> simplify_expr(T) end, List1),
	    List21 = lists:map(fun(T) -> simplify_expr(T) end, List2),
	    case lists:prefix(List11, List21) of 
		true ->
		    List22 = lists:sublist(List2, Len1),
		    BdList1 = var_binding_structure(List1),
		    BdList2 = var_binding_structure(List22),
		    case BdList1 == BdList2 of 
			true -> E1 = hd(List22),
				En = lists:last(List22),
			        {{StartLn, StartCol}, _EndLoc} = refac_util:get_range(E1),
				{_StartLoc1, {EndLn, EndCol}} = refac_util:get_range(En),
				[{StartLn, StartCol, EndLn, EndCol+1}] ++ get_clone(List1, tl(List2));
			_ -> get_clone(List1, tl(List2))
		    end;				       
		    _ -> get_clone(List1, tl(List2))
		end;			
	_  -> []
    end.
	    

%% simplify an expression by masking variable names and literal variables, and also replace
%% the concrete location values with the default value.
simplify_expr(Exp) ->
    refac_util:full_buTP(fun(Node,_Others)->do_simplify_expr(Node) end, Exp,{}).

do_simplify_expr(Node) ->
    Node1 = case refac_syntax:type(Node) of 
		macro -> 
		    refac_syntax:default_literals_vars(Node, '*');
		variable ->
		    refac_syntax:default_literals_vars(Node, '&');
		integer ->
		    refac_syntax:default_literals_vars(Node, 0);
		float ->
		    refac_syntax:default_literals_vars(Node, 0);
		char ->
		    refac_syntax:default_literals_vars(Node, '%');
		string ->
		    refac_syntax:default_literals_vars(Node, '%');
		atom -> case lists:keysearch(fun_def,1,refac_syntax:get_ann(Node)) of 
			    false ->refac_syntax:default_literals_vars(Node, '%') ;
			    _ -> refac_syntax:default_literals_vars(Node, refac_syntax:atom_value(Node))
			end;
		nil -> refac_syntax:default_literals_vars(Node, nil);
		underscore ->refac_syntax:default_literals_vars(Node, '&');
		_ ->
		    Node
	    end,
    set_default_ann(Node1).
			  

set_default_ann(Node) ->
    refac_syntax:set_pos(refac_syntax:remove_comments(refac_syntax:set_ann(Node, [])), {0,0}).

%% get all the expression sequences contained in Tree.	    
contained_exprs(Tree, MinLen) ->
    F = fun(T, Acc) ->
		case refac_syntax:type(T) of
		    clause -> Exprs = refac_syntax:clause_body(T),  %% HOW ABOUT CLAUSE_GUARD?
			     Acc ++ [Exprs];
		    application -> Exprs = refac_syntax:application_arguments(T),
			     Acc++ [Exprs];
		    tuple -> Exprs = refac_syntax:tuple_elements(T),
			     Acc++ [Exprs];
		    lists -> Exprs = refac_syntax:list_prefix(T),
			     Acc++ [Exprs];
		    block_expr -> Exprs = refac_syntax:block_expr_body(T),
			      Acc++ [Exprs];    
		    _  -> Acc
		end
	end,
    Es = refac_syntax_lib:fold(F, [], Tree),
    [E ||  E <- Es, length(E) >= MinLen].
		 


      

%% get the binding structure of variables.
%%-spec(var_binding_structure/1::([syntaxTree()]) -> [{integer(), integer()}]).      
var_binding_structure(ASTList) ->
    Fun1 = fun (T, S) ->
		   case refac_syntax:type(T) of
		     variable ->
			 Name = refac_syntax:variable_name(T),
			 SrcLoc = refac_syntax:get_pos(T),
			 As = refac_syntax:get_ann(T),
			 case lists:keysearch(def, 1, As) of
			   {value, {def, DefLoc}} ->
			       ordsets:add_element({atom_to_list(Name), SrcLoc,
						    DefLoc},
						   S);
			   _ -> S
			 end;
		     _ -> S
		   end
	   end,
    %% collect all variables including their defining and occuring locations. 
    B = lists:flatmap(fun(T) -> refac_syntax_lib:fold(Fun1, ordsets:new(), T) end, ASTList),
    %% collect defining locations.
  %%  DefLocs = lists:usort(lists:flatten(lists:map(fun ({_Name, _SrcLoc, DefLoc}) ->  DefLoc end, B))),
    %% collect occuring locations.
    SrcLocs = lists:map(fun ({_Name, SrcLoc, _DefLoc}) -> SrcLoc end, B),
    Res = case SrcLocs of 
	      [] -> 
		  [];
	      _ ->  IndexedLocs = lists:zip(SrcLocs, lists:seq(1, length(SrcLocs))),
		    B1 = lists:usort(lists:map(fun ({_Name, SrcLoc, DefLoc}) ->
						       DefLoc1 = hd(DefLoc),             %% DefLoc is  a list, does this cause problems? .
						       {value, {SrcLoc, Index1}} = lists:keysearch(SrcLoc, 1, IndexedLocs),
						       Index2 = case lists:keysearch(DefLoc1, 1, IndexedLocs) of 
								    {value, {_, Ind}} -> Ind;
								    _ -> 0 %% free variable
								end,
						       {Index1, Index2}
					       end,
					       B)),
		    lists:keysort(1, B1)
	  end,
    Res.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Refactoring: Normalise record expression.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(normalise_record_expr/4::(filename(), pos(), [dir()], integer()) -> {error, string()} | {ok, [filename()]}).
normalise_record_expr(FName, Pos={Line, Col}, SearchPaths, TabWidth) ->
    ?wrangler_io("\nCMD: ~p:normalise_record_expr(~p, {~p,~p},~p, ~p).\n", [?MODULE, FName, Line, Col, SearchPaths,TabWidth]),
    {ok, {AnnAST, Info}} =refac_util:parse_annotate_file(FName,true, [], TabWidth),
    RecordExpr =pos_to_record_expr(AnnAST, Pos),
    {AnnAST1, _Changed} = normalise_record_expr_1(FName, AnnAST, Info, Pos, RecordExpr,  SearchPaths, TabWidth),
    refac_util:write_refactored_files_for_preview([{{FName, FName}, AnnAST1}]),
    {ok, [FName]}.


normalise_record_expr_1(FileName, AnnAST, Info, Pos, RecordExpr, SearchPaths, TabWidth) ->
    RecordTypes1 = collect_record_types(RecordExpr),
    RecordTypes = lists:flatmap(fun(T) -> case refac_syntax:type(T) of 
					      atom-> [refac_syntax:concrete(T)];
					      _ -> []
					  end
				end, RecordTypes1),
    case length(RecordTypes) < length(RecordTypes1) of 
	true -> ?wrangler_io("Warning: record expressions with a non-atom record type are not normalised by this refactoring.",[]);
	_ -> ok
    end,
    case RecordTypes of 
	[] -> throw({error, "No record expression to normalise."});
	_ ->
      	    RecordsDefinedInHrl = case lists:keysearch(records, 1, Info) of 
				      {value, {records, RecordDefs}} ->
					  RecordsDefinedInMod = lists:map(fun({Name, _Def}) -> Name end, RecordDefs),
					  lists:subtract(RecordTypes, RecordsDefinedInMod);
				      _ -> RecordTypes
				  end,
	    RecordInfo = case RecordsDefinedInHrl of 
			     [] ->  {value, {records, Records}} = lists:keysearch(records,1, Info),
				    Records;
			     _ -> {ok, {_AnnAST1, Info1}} =refac_util:parse_annotate_file(FileName,false,SearchPaths, TabWidth),
				  case lists:keysearch(records,1, Info1) of 
				      {value, {records, Records}} -> Records;
				      _ ->[]
				  end
			 end,
 			     
	    refac_util:stop_tdTP(fun do_normalise_record_expr/2, AnnAST, {Pos,RecordInfo})
    end.

do_normalise_record_expr(Node, {Pos, RecordInfo}) ->
    case refac_syntax:type(Node) of 
	record_expr ->
	    {S, E} = refac_util:get_range(Node), 
	    case (S =<Pos) andalso (Pos =< E) of 
		true ->  Node1 =refac_util:full_buTP(fun do_normalise_record_expr_1/2, Node, RecordInfo),
			 {Node1, true};
		_ -> {Node, false}
	    end;
	_ -> {Node, false}
    end.

do_normalise_record_expr_1(Node, RecordInfo) ->
    Fun = fun({{FName, FVal}, Fields}) ->
		  case lists:filter(fun(F1) -> T1 = refac_syntax:record_field_name(F1), 
					       case refac_syntax:type(T1) of 
						   atom -> refac_syntax:concrete(refac_syntax:record_field_name(F1))== FName;
						   _ -> false
					       end
				    end, Fields) of 
		      [F2|_] -> F2;
		      _ ->
			  case lists:filter(fun(F1) ->
					  refac_syntax:type(refac_syntax:record_field_name(F1))== underscore end, Fields) of 
			      [F2|_] ->
				  refac_syntax:record_field(refac_syntax:atom(FName), refac_syntax:record_field_value(F2));
			      _ ->
				  case FVal of 
				      none -> refac_syntax:record_field(refac_syntax:atom(FName),refac_syntax:atom(undefined));
				      _ ->
					  refac_syntax:record_field(refac_syntax:atom(FName), FVal)
				  end
			  end
		  end	  			 
	  end,	  
    case refac_syntax:type(Node) of 
	record_expr ->
	    Arg = refac_syntax:record_expr_argument(Node),
	    Type = refac_syntax:record_expr_type(Node), 
	    Fields = refac_syntax:record_expr_fields(Node),
	    case refac_syntax:type(Type) of 
		atom -> 
		    Type1 = refac_syntax:concrete(Type),
		    case lists:keysearch(Type1, 1, RecordInfo) of 
			{value, {Type1, Fields1}} ->
			    Fields2 = lists:map(fun(F) -> {F, Fields} end, Fields1),
			    NormalisedFields =lists:map(Fun, Fields2),
			    refac_syntax:record_expr(Arg, Type, NormalisedFields);
			_ -> 
			    ?wrangler_io("Warning: wrangler could not find the definition of record '~p'.",[Type1]),
			    Node
		    end;
		_ -> Node
	    end;
	_ -> Node
    end.
    
	    
collect_record_types(Tree) ->
    F = fun(T,S) ->
		case refac_syntax:type(T) of
		    record_expr ->
			S ++ [refac_syntax:record_expr_type(T)];
		    _ -> S
		end
	end,
    refac_syntax_lib:fold(F, [], Tree).

pos_to_record_expr(Tree, Pos) ->
    case refac_util:once_tdTU(fun pos_to_record_expr_1/2, Tree, Pos) of 
	{_, false} ->
	     throw({error, "You have not selected a record expression"});
	{R, true} -> 
	    R
    end.

pos_to_record_expr_1(Node, Pos) ->
    case refac_syntax:type(Node) of 
	record_expr ->
	    {S, E} = refac_util:get_range(Node), 
	    case (S =<Pos) andalso (Pos =< E) of 
		true -> {Node, true};
		_ -> {[], false}
	    end;
	_ -> {[], false}
    end.
