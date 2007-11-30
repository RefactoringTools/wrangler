%% =====================================================================
%% Some utility functions used by the refactorer.
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
%% =====================================================================
%%
%% @doc Some utility functions used by the refactorer.
%%
%% @end
%% ======================================================================

-module(refac_util).

-export([ build_sideeffect_tab/3, build_sideeffect_tab/1,side_effect_table/0,
	 expr_to_fun/2, envs_bounds_frees/1, 
	 full_buTP/2,full_buTP/3, get_bound_vars/1, 
	 get_free_vars/1, get_range/1, get_var_exports/1, 
	 has_side_effect/3, inscope_funs/1, 
	 is_expr/1,is_pattern/1,is_fun_name/1, is_var_name/1, once_tdTU/3, 
	 parse_annotate_file/2,
	 parse_annotate_file/4,
	 pos_to_expr/3, pos_to_fun_name/2, pos_to_fun_def/2, fun_to_def_pos/2,
	 pos_to_var_name/2, reset_attrs/1, stop_tdTP/3, stop_tdTP1/3,
	 update_ann/2, write_refactored_files/1, is_exported/2,get_client_files/2,
         collect_var_mod_qualifiers/1,
	 callback_funs/1, expand_files/2, get_modules_by_file/1,
	 to_lower/1, to_upper/1,
	 post_refac_check/3,
	 try_evaluation/1]).
       
-define(DEFAULT_LOC, 
        {0, 0}).  %% default defining location.
-define(DEFAULT_MODULE,
	unknown).  %% default module name.

-record(attr, {pos = {0,0}, ann = [], com = none}).

-include("../hrl/wrangler.hrl").


ghead(Info, []) ->
    erlang:error(Info);
ghead(_Info, L ) -> hd(L).


glast(Info, []) ->
    erlang:error(Info);
glast(_Info, L) -> lists:last(L).
%% =====================================================================
%% 
%% @spec once_tdTU(Function, Tree::syntaxTree(), Others::[term()])-> syntaxTree()
%%      Function = (Tree, Others) -> Tree1
%% @doc Once-topdown type-unifying traversal of the abstract syntax tree with some 
%% information collected.
%% 
%% @see full_buTP/2
%% @see stop_tdTP/3 		

once_tdTU(Fun, Node,Others) -> 
      case Fun(Node,Others) of  
 	 {R, true} -> {R, true};
 	 {_R, false}     -> 
 	     case erl_syntax:subtrees(Node) of 
 		 [] -> {[], false};
 		 Gs -> Flattened_Gs = [ T || G <- Gs, T<-G],
 		       case Flattened_Gs of 
 			   []-> {[], false};
 			   [H |T1] -> until(Fun, [H|T1], Others)
		       end
 	     end
      end.
		
	   
until(_F, [], _Others) -> {[], false};
until(F, [H|T],Others) ->
	     case once_tdTU(F, H,Others) of 
                  {_R, true} -> {_R, true};
 		 {_Rq, false} -> until(F, T, Others)
 	     end.


%% =====================================================================
%% 
%% @spec stop_tdTP(Function, Tree::syntaxTree(), Others::[term()])-> syntaxTree()
%%      Function = (Tree, Others) -> Tree1
%% @doc Stop-topdown type-preserving traversal of the abstract syntax tree.
%% 
%% @see full_buTP/2
%% @see once_tdTU/3 

stop_tdTP(Fun, Node, Others) ->
     case Fun(Node, Others) of
       {Node1, true} -> Node1;
       {Node1, false} ->
 	  case refac_syntax:subtrees(Node1) of
 	    [] -> Node1;
 	    Gs ->
 		Gs1 = [[stop_tdTP(Fun, T, Others) || T <- G]
 		       || G <- Gs],
 		Node2 = refac_syntax:make_tree(refac_syntax:type(Node1), Gs1),
 		refac_syntax:copy_attrs(Node1, Node2)
 	  end
     end.

stop_tdTP1(Fun, Node, Others) ->
    case Fun(Node, Others) of
      {Node1, true} -> {Node1,true};
      {Node1, false} ->
	  case refac_syntax:subtrees(Node1) of
	    [] -> {Node1, false};
	    Gs ->
		Gs1 = [[stop_tdTP1(Fun, T, Others) || T <- G]
		       || G <- Gs],
		Gs2 = [[N ||{N, _B}<- G] || G <- Gs1],
		G =   [[B ||{_N, B} <- G] || G <- Gs1],
		Node2 = refac_syntax:make_tree(refac_syntax:type(Node1), Gs2),
		{refac_syntax:copy_attrs(Node1, Node2), lists:member(true, lists:flatten(G))}
	  end
    end.


%% =====================================================================
%% 
%% @spec full_buTP(Function, Tree::syntaxTree())-> syntaxTree()
%%      Function = (Tree) -> Tree1
%%
%% @doc Full bottom-up type-preserving traversal of the abstract syntax tree.
%% 
%% @see stop_tdTP/2
%% @see once_tdTU/3 
full_buTP (Fun, Tree) ->
   case refac_syntax:subtrees(Tree) of 
	[] -> Fun(Tree);
	Gs -> Gs1 = [[full_buTP (Fun, T) || T <-G] || G <- Gs],
	      Tree1 = refac_syntax:make_tree (refac_syntax:type(Tree), Gs1),
	      Fun(refac_syntax:copy_attrs(Tree, Tree1))
    end.



full_buTP (Fun, Tree, Others) ->
   case refac_syntax:subtrees(Tree) of 
	[] -> Fun(Tree, Others);
	Gs -> Gs1 = [[full_buTP (Fun, T, Others) || T <-G] || G <- Gs],
	      Tree1 = refac_syntax:make_tree (refac_syntax:type(Tree), Gs1),
	      Fun(refac_syntax:copy_attrs(Tree, Tree1), Others)
    end.
%% =====================================================================
%% 
%% @spec pos_to_fun_name(Node::syntaxTree(), Pos::{integer(), integer()}) -> {ok, atom()} | {error, string()}
%%    
%% @doc Get the function name that occurs at the specified position in the program source.
%% @see pos_to_var_name/2 
%% @see pos_to_expr/3
   
pos_to_fun_name(Node, Pos) ->  
    case once_tdTU(fun pos_to_fun_name_1/2, Node, Pos) of 
	{_, false} -> {error, "You have not selected a function name."};
	{R, true} -> {ok, R}
    end.
				  
pos_to_fun_name_1(Node,Pos={Ln,Col}) ->
      As = refac_syntax:get_ann(Node),
      case lists:keysearch(fun_def, 1, As) of 
  	{value, {fun_def, {Mod, Fun, Arity, {Ln, Col1}, DefPos}}} ->
	    case (Col1=<Col) and (Col =<Col1+length(atom_to_list(Fun))-1) of
		true -> {{Mod, Fun, Arity, Pos, DefPos}, true};
		false -> {[], false}
	    end;
  	_  -> {[], false}
      end.

pos_to_fun_def(Node, Pos) ->
    case once_tdTU(fun pos_to_fun_def_1/2, Node, Pos) of
	{_, false} -> {error, "You have not selected a function definition."};
	{R, true} -> {ok, R}
    end.

pos_to_fun_def_1(Node, Pos) ->
    case refac_syntax:type(Node) of 
	function ->
	    {S, E} = get_range(Node),
	    if (S=<Pos) and (Pos =< E) ->
		    As = refac_syntax:get_ann(Node),
		    case lists:keysearch(fun_def, 1, As) of 
			{value, {fun_def, {Mod, FunName, Arity, _Pos1, _Pos2}}} ->
			    {{Mod,FunName, Arity, Node}, true};
			_ -> {[], false}
		    end;
	       true -> {[], false}
	    end;
	_ ->
	    {[], false}
    end.
		    
    
	    

fun_to_def_pos(Node, {Mod, FunName, Arity}) ->
    case once_tdTU(fun fun_to_def_pos_1/2, Node, {Mod, FunName, Arity}) of
	{_, false} ->
	     {error, "The function is not defined in this node."};
	{R, true}-> {ok, R}
    end.
fun_to_def_pos_1(Node, {Mod, FunName, Arity}) ->
    case refac_syntax:type(Node) of 
	function -> As = refac_syntax:get_ann(Node),
		    case lists:keysearch(fun_def, 1, As) of 
			{value, {fun_def, {Mod, FunName, Arity, _P, DefinePos}}} ->
			    {DefinePos, true};
			_  -> {[],false}
		    end;
	 _ -> {[], false}
    end.
	    
	    

%% =====================================================================
%% 
%% @spec pos_to_var_name(Node::syntaxTree(), Pos::Pos) -> {ok, {atom(), {Pos, Pos}}} | {error, string()}
%%    
%% @doc Get the variable name that occurs at the specified position in the program source.
%% @see pos_to_fun_name/2
%% @see pos_to_expr/3 

pos_to_var_name(Node, UsePos)->
    case once_tdTU(fun pos_to_var_name_1/2, Node, UsePos) of 
	{_, false} -> {error, "You have not selected a variable name."}; 
        {R, true}  -> {ok, R}
    end.

pos_to_var_name_1(Node, Pos={Ln, Col}) ->
    case refac_syntax:type(Node) of 
	variable ->
	    {Ln1,Col1} = refac_syntax:get_pos(Node), 
	    case  (Ln==Ln1) and (Col1=<Col) and (Col =<Col1+length(atom_to_list(refac_syntax:variable_name(Node)))-1) of 
		true -> 
		    case lists:keysearch(def, 1, refac_syntax:get_ann(Node)) of 
			{value, {def,DefinePos}} -> 
			    lists:keysearch(def, 1,refac_syntax:get_ann(Node)),
			    {value, {category, C}} = lists:keysearch(category, 1, refac_syntax:get_ann(Node)),
			    {{refac_syntax:variable_name(Node), {Pos, DefinePos},C}, true};
			false ->
			    {value, {category, C}} = lists:keysearch(category, 1, refac_syntax:get_ann(Node)),
			    {{refac_syntax:variable_name(Node), {Pos, ?DEFAULT_LOC}, C}, true}
		    end;
		false -> {[], false}
	    end;
	_  -> {[], false}
    end.
%% =====================================================================
%% 
%% @spec pos_to_expr(Tree::syntaxTree(), Start::Pos, End::Pos) -> {ok, syntaxTree()} | {error, string()}
%%    
%% @doc Get the largest, left-most expression enclosed by the start and end positions.
%% @see pos_to_fun_name/2
%% @see pos_to_var_name/2 

%% TODO: CHANGE THE RETURN TYPE OF THIS FUNCTION.
pos_to_expr(Tree, Start, End) ->
    {S, E} = get_range(Tree),
    if (S >= Start) and (E=<End) -> case is_expr(Tree) of 
					true -> [Tree];
					_    -> Ts = refac_syntax:subtrees(Tree),
					        R0 = [[pos_to_expr(T, Start, End) || T <-G] || G <- Ts],
						lists:flatten(R0)
				    end;
       (S > End) or (E<Start) -> [];
       (S <Start) or (E > End) -> Ts = refac_syntax:subtrees(Tree),
				  R0 = [[pos_to_expr(T, Start, End)|| T <-G] || G<-Ts],
				  lists:flatten(R0);
       true  -> []
    
    end.


post_refac_check(FileName, AST, SearchPaths) ->
    TempFileName = filename:join([filename:dirname(FileName),
				  filename:rootname(filename:basename(FileName))++"_refac_temp"]),
    file:write_file(TempFileName, list_to_binary(refac_prettypr:format(AST))),
    case refac_epp:parse_file(TempFileName, SearchPaths,[]) of
	{ok,  Forms1} 
	  -> Forms = refac_syntax:form_list(tl(Forms1)),
             AnnAST = refac_syntax_lib:annotate_bindings(Forms, ordsets:new()),
	     case analyze_free_vars(AnnAST) of 
		 {error, _Reason} -> 
		     file:delete(TempFileName),
		     error;
		  _ ->
		     file:delete(TempFileName),
		     ok
	     end;
	{error, _Reason} ->
	    file:delete(TempFileName),
	    error
    end.
		
    
%% =====================================================================
%% 
%% @spec parse_annotate_file(FName::filename())-> {ok, syntaxTree()} | {error, Reason}
%%    
%% @doc Parse an Erlang file, and annotate the abstract syntax tree with static semantic information.

parse_annotate_file(FName, AnnotateLevel) ->
    parse_annotate_file(FName, AnnotateLevel, true, []).

parse_annotate_file(FName,AnnotateLevel,ByPassPreP,SearchPaths) ->
    R = case ByPassPreP of 
	    true -> refac_epp_dodger:parse_file(FName);
	    false ->refac_epp:parse_file(FName, SearchPaths,[])
	end,
    case R of 
       {ok, Forms1} ->
 	  Forms = if ByPassPreP -> Forms1;
		     true -> tl(Forms1)
		  end,
          Comments = erl_comment_scan:file(FName),
 	  SyntaxTree = refac_recomment:recomment_forms(Forms, Comments),
   	  Info= refac_syntax_lib:analyze_forms(SyntaxTree),  
	  case lists:keysearch(errors,1,Info) of
	      {value,{errors, Error}} -> 
		  {error, {"Syntax error in file: " ++ FName++".", Error}};
	      _ -> AnnAST = annotate_bindings(FName, SyntaxTree, Info, AnnotateLevel),
		   if ByPassPreP ->
			   {ok, {AnnAST, Info}};
		      true -> case analyze_free_vars(AnnAST) of 
				  {error, Reason} -> {error, Reason};
				  _ -> {ok, {AnnAST, Info}}
			      end
		   end
	  end;
	{error, Reason} -> {error, Reason}                               
    end.

analyze_free_vars(SyntaxTree) ->
    Ann =  refac_syntax:get_ann(SyntaxTree),
    case lists:keysearch(free, 1, Ann) of 
	{value, {free, FrVars}} -> 
	    case FrVars of 
		[] ->
		     ok;
		Ls -> {error,"Unbound variable(s) found: " ++ show_fv_vars(Ls)}
	    end;
	_  -> ok
    end.

show_fv_vars([]) ->
     ".";
show_fv_vars([{A,{Line, Col}}|T]) ->
    T1 = if T==[] -> ".";
	    true ->  ", "++show_fv_vars(T)
	 end,
    atom_to_list(A) ++ " at: {" ++ integer_to_list(Line) ++ ","++ integer_to_list(Col)++"}" 
	++ T1.


%% Annotate the AST with static semantic information.
annotate_bindings(FName, AST, Info, AnnotateLevel) ->
    AnnAST0 = refac_syntax_lib:annotate_bindings(AST, ordsets:new()),
    AnnAST1 = update_var_define_locations(AnnAST0),
    AnnAST2 = add_category(AnnAST1),
    case AnnotateLevel of 
	0 -> AnnAST2;
	1 -> AnnAST3 =adjust_locations(FName, AnnAST2),
	     add_fun_define_locations(AnnAST3, Info);
	2 ->AnnAST3 =adjust_locations(FName, AnnAST2),
	    AnnAST4 = add_fun_define_locations(AnnAST3, Info),
	    add_range(FName,AnnAST4)
    end.

add_range(FName, AST) ->
      {ok, Toks} = refac_epp:scan_file(FName, [],[]),
      full_buTP(fun do_add_range/2, AST, Toks).


do_add_range(Node, Toks) ->
     {L,C} = refac_syntax:get_pos(Node),
     case refac_syntax:type(Node) of
	 variable ->
	     Len = length(refac_syntax:variable_literal(Node)),
	     refac_syntax:add_ann({range, {{L,C}, {L, C+Len-1}}}, Node);
	 atom -> 
	     Len = length(atom_to_list(refac_syntax:atom_value(Node))),
	     refac_syntax:add_ann({range, {{L,C},{L,C+Len-1}}}, Node);	
	 operator ->
	     Len = length(atom_to_list(refac_syntax:atom_value(Node))),
	     refac_syntax:add_ann({range, {{L,C},{L,C+Len-1}}}, Node);
	 char -> 
	     refac_syntax:add_ann({range, {{L,C}, {L, C}}}, Node);
	 integer ->
	     Len = length(refac_syntax:integer_literal(Node)),
	     refac_syntax:add_ann({range, {{L,C}, {L,C+Len-1}}}, Node);	     
	 string ->
	     Len = length(refac_syntax:string_literal(Node)),
	     refac_syntax:add_ann({range, {{L,C}, {L,C+Len-1}}}, Node);
	 float ->
	     refac_syntax:add_ann({range, {{L,C}, {L,C}}}, Node); %% This is problematic.
	 underscore ->
             refac_syntax:add_ann({range, {{L,C}, {L, C}}}, Node);
	 eof_marker -> 
	     refac_syntax:add_ann({range, {{L,C},{L,C}}}, Node);
	 nil ->refac_syntax:add_ann({range, {{L,C}, {L, C+1}}}, Node);
         module_qualifier -> M = refac_syntax:module_qualifier_argument(Node),
			     F = refac_syntax:module_qualifier_body(Node),
			     {S1, _E1} = get_range(M),
			     {_S2, E2} = get_range(F),
			     refac_syntax:add_ann({range,{S1, E2}}, Node);

	 list  ->  LP = ghead("refac_util:do_add_range,list",refac_syntax:list_prefix(Node)),
		   {{L1,C1}, {L2,C2}} = get_range(LP),
		   Node1 = case refac_syntax:list_suffix(Node) of
			       none -> refac_syntax:add_ann({range, {{L1, C1-1}, {L2, C2+1}}}, Node);
			       Tail -> {_S2, {L3,C3}} = get_range(Tail),
				       refac_syntax:add_ann({range, {{L1,C1-1},{L3, C3}}}, Node)
			   end,
	           Node1;
	 application -> O = refac_syntax:application_operator(Node),
			Args = refac_syntax:application_arguments(Node),
			{S1, E1} = get_range(O),
			{S3,E3} = case Args of 
			      [] -> {S1, E1};
			      _ ->  La = glast("refac_util:do_add_range, application", Args),
				    {_S2, E2} = get_range(La),
				    {S1, E2}
			    end,
			E31 = extend_backwards(Toks, E3, ')'),
			refac_syntax:add_ann({range, {S3, E31}}, Node);
	 case_expr -> A = refac_syntax:case_expr_argument(Node),
		      Lc= glast("refac_util:do_add_range,case_expr", refac_syntax:case_expr_clauses(Node)),
		      {S1, _E1} = get_range(A),
		      {_S2, E2} = get_range(Lc),
                      S11 = extend_forwards(Toks, S1, 'case'),
		      E21 = extend_backwards(Toks, E2, 'end'),
		      refac_syntax:add_ann({range, {S11, E21}}, Node);
	 clause -> P = refac_syntax:get_pos(Node),
		   Body = glast("refac_util:do_add_range, clause", refac_syntax:clause_body(Node)),
		   {_S2, E2} = get_range(Body),
		   refac_syntax:add_ann({range, {P, E2}}, Node);
	 catch_expr -> B = refac_syntax:catch_expr_body(Node),
		       {S, E} = get_range(B),
		       S1 = extend_forwards(Toks, S, 'catch'),
		       refac_syntax:add_ann({range, {S1, E}}, Node);
	 if_expr  -> Cs = refac_syntax:if_expr_clauses(Node),
		     Hd = ghead("refac_util:do_add_range, if_expr", Cs),
		     La = glast("refac_util:do_add_range, if_expr", Cs),
		     {S1, _E1} = get_range(Hd),
		     {_S2, E2} = get_range(La),
		     S11 = extend_forwards(Toks,S1, 'if'),
		     E21 = extend_backwards(Toks, E2, 'end'),
		     refac_syntax:add_ann({range, {S11, E21}}, Node);
	 cond_expr -> Cs = refac_syntax:cond_expr_clauses(Node),
		      Hd = ghead("refac_util:do_add_range, cond_expr",Cs),
		      La = glast("refac_util:do_add_range, cond_expr", Cs),
		      {S1, _E1} = get_range(Hd),
		      {_S2, E2} = get_range(La),
		      S11 = extend_forwards(Toks,S1, 'cond'),
		      E21 = extend_backwards(Toks, E2, 'end'),
		      refac_syntax:add_ann({range, {S11, E21}}, Node);
	 infix_expr -> Left = refac_syntax:infix_expr_left(Node),
		       Right = refac_syntax:infix_expr_right(Node),
		       {S1, _E1} = get_range(Left),
		       {_S2, E2} = get_range(Right),
                       refac_syntax:add_ann({range, {S1, E2}}, Node);
	 prefix_expr -> Op = refac_syntax:prefix_expr_operator(Node),
			Ar = refac_syntax:prefix_expr_argument(Node),
			{S1, _E1} = get_range(Op),
			{_S2, E2} = get_range(Ar),
			E21 = extend_backwards(Toks, E2, ')'),
			refac_syntax:add_ann({range, {S1, E21}}, Node);
	 conjunction -> B = refac_syntax:conjunction_body(Node),
			H =ghead("refac_util:do_add_range,conjunction",B),
			La = glast("refac_util:do_add_range,conjunction",B),
			{S1, _E1} = get_range(H),
			{_S2, E2} = get_range(La),
			refac_syntax:add_ann({range, {S1, E2}}, Node);
	 disjunction -> B = refac_syntax:disjunction_body(Node),
			H =ghead("refac_util:do_add_range, disjunction", B),
			La = glast("refac_util:do_add_range,disjunction",B),
			{S1, _E1} = get_range(H),
			{_S2, E2} = get_range(La),
			refac_syntax:add_ann({range, {S1, E2}}, Node);
	 function ->  F = refac_syntax:function_name(Node),
		      Cs = refac_syntax:function_clauses(Node),
		      Lc = glast("refac_util:do_add_range,function", Cs),
		      {S1, _E1} = get_range(F),
		      {_S2, E2} = get_range(Lc),
		      refac_syntax:add_ann({range, {S1, E2}}, Node);
	 fun_expr -> Cs = refac_syntax:fun_expr_clauses(Node),
		     S = refac_syntax:get_pos(Node),
		     Lc = glast("refac_util:do_add_range, fun_expr", Cs),
		     {_S1, E1} = get_range(Lc),
		     E11 = extend_backwards(Toks, E1, 'end'),   %% S starts from 'fun', so there is no need to extend forwards/
		     refac_syntax:add_ann({range, {S, E11}}, Node);
	arity_qualifier -> B = refac_syntax:arity_qualifier_body(Node),
			   A = refac_syntax:arity_qualifier_argument(Node),
			   {S1, _E1} = get_range(B),
			   {_S2, E2} = get_range(A),
			   refac_syntax:add_ann({range, {S1, E2}}, Node);
	 implicit_fun -> S =refac_syntax:get_pos(Node),
                         N = refac_syntax:implicit_fun_name(Node),
			 {_S1, E1} = get_range(N),
			 refac_syntax:add_ann({range, {S, E1}}, Node);
	 attribute -> Name = refac_syntax:attribute_name(Node),
		      Arg = glast("refac_util:do_add_range,attribute",refac_syntax:attribute_arguments(Node)),
 		      {S1, _E1} = get_range(Name),
		      {_S2, E2} = get_range(Arg),
		      refac_syntax:add_ann({range, {S1, E2}},Node);
	 generator -> P = refac_syntax:generator_pattern(Node),
		      B = refac_syntax:generator_body(Node),
		      {S1, _E1} = get_range(P),
		      {_S2, E2} = get_range(B),
		      refac_syntax:add_ann({range, {S1, E2}}, Node);
	 tuple    ->  Es = refac_syntax:tuple_elements(Node),
		      case length(Es) of 
			  0 -> refac_syntax:add_ann({range, {{L,C}, {L, C+1}}}, Node);
			  _ -> Hd = ghead("refac_util:do_add_range, tuple", Es),
			       La = glast("refac_util:do_add_range, tuple", Es),
			       {S1, _E1}= get_range(Hd),
			       {_S2, E2} = get_range(La),
			       S11 = extend_forwards(Toks, S1, '{'),
		               E21 = extend_backwards(Toks, E2, '}'),
			       refac_syntax:add_ann({range, {S11, E21}}, Node)
		      end;
	 list_comp -> T = refac_syntax:list_comp_template(Node),
		      B = glast("refac_util:do_add_range,list_comp", refac_syntax:list_comp_body(Node)),
                      {S1, _E1} = get_range(T),
		      {_S2, E2} = get_range(B),
		      S11 = extend_forwards(Toks, S1, '['),
		      E21 = extend_backwards(Toks, E2, ']'),
		      refac_syntax:add_ann({range, {S11, E21}}, Node);
	 block_expr ->Es = refac_syntax:block_expr_body(Node),
		      Hd = ghead("refac_util:do_add_range, block_expr", Es),
		      La = glast("refac_util:do_add_range, block_expr", Es),
		      {S1, _E1}= get_range(Hd),
		      {_S2, E2} = get_range(La),
		      S11 = extend_forwards(Toks, S1, 'begin'),
		      E21 = extend_backwards(Toks, E2, 'end'),
		      refac_syntax:add_ann({range, {S11, E21}}, Node);
	 receive_expr -> case refac_syntax:receive_expr_timeout(Node) of 
			     none -> Cs = refac_syntax:receive_expr_clauses(Node),
				     case length(Cs) of 
					 0 -> refac_syntax:add_ann({range, {L, C}, {L,C}}, Node);
					 _ -> Hd = ghead("refac_util:do_add_range, receive_expr1",Cs),
					      La = glast("refac_util:do_add_range, receive_expr1",Cs),
					      {S1, _E1} = get_range(Hd),
					      {_S2, E2} = get_range(La),
					      S11 = extend_forwards(Toks, S1, 'receive'),
					      E21 = extend_backwards(Toks, E2, 'end'),	           
					      refac_syntax:add_ann({range, {S11, E21}}, Node)
					 end;
			     _E ->
				 Cs = refac_syntax:receive_expr_clauses(Node),
				 A = refac_syntax:receive_expr_action(Node),
				 case length(Cs) of 
				     0 -> {_S2, E2} = get_range(glast("refac_util:do_add_range, receive_expr2",A)),
					  refac_syntax:add_ann({range, {{L,C}, E2}}, Node);
				     _ -> Hd = ghead("refac_util:do_add_range,receive_expr2",Cs),
					  {S1, _E1} = get_range(Hd),
					  {_S2, E2} = get_range(glast("refac_util:do_add_range, receive_expr3", A)),
					  S11 = extend_forwards(Toks, S1, 'receive'),
					  E21 = extend_backwards(Toks, E2, 'end'),
					  refac_syntax:add_ann({range, {S11,E21}}, Node)
				 end
			     end;
	 binary -> Fs = refac_syntax:binary_fields(Node),
		   case Fs == [] of 
		       true -> refac_syntax:add_ann({range, {{L,C}, {L, C+3}}}, Node);
		       _ -> Hd = ghead("refac_util:do_add_range, binary",Fs),
			    La = glast("refac_util:do_add_range, binary",Fs),
			    {S1, _E1} = get_range(Hd),
			    {_S2, E2} = get_range(La),
			    S11 = extend_forwards(Toks, S1, '<<'),
			    E21 = extend_backwards(Toks, E2, '>>'),
			    refac_syntax:add_ann({range, {S11, E21}}, Node)
		   end;
	 binary_field -> Body = refac_syntax:binary_field_body(Node),
			 Types =refac_syntax:binary_field_types(Node),
			 {S1, E1} = get_range(Body),
			 {_S2, E2} = if Types == [] -> {S1, E1};
					true -> get_range(glast("refac_util:do_add_range,binary_field",Types))
				     end,
			 refac_syntax:add_ann({range,{S1,E2}}, Node);
	 match_expr -> P = refac_syntax:match_expr_pattern(Node),
		       B = refac_syntax:match_expr_body(Node),
		       {S1, _E1} = get_range(P),
		       {_S2, E2} = get_range(B),
		       refac_syntax:add_ann({range, {S1, E2}}, Node);
	 form_list -> Es = refac_syntax:form_list_elements(Node),
		      Hd = ghead("refac_util:do_add_range, form_list",Es),
		      La = glast("refac_util:do_add_range, form_list", Es),
		      {S1, _E1}= get_range(Hd),
		      {_S2, E2} = get_range(La),
		      refac_syntax:add_ann({range, {S1, E2}}, Node);     
	 parentheses -> B = refac_syntax:parentheses_body(Node),
			{S, E} = get_range(B),
			S1 = extend_forwards(Toks, S, '('),
			E1 = extend_backwards(Toks, E, ')'),
			refac_syntax:add_ann({range, {S1,E1}}, Node);
	 class_qualifier -> A = refac_syntax:class_qualifier_argument(Node),
			    B = refac_syntax:class_body(Node),
			    {S1, _E1} = get_range(A),
			    {_S2, E2} = get_range(B),
			    refac_syntax:add_ann({range, {S1,E2}}, Node);
	 qualified_name -> Es = refac_syntax:qualified_name_segments(Node),
			   Hd = ghead("refac_util:do_add_range, qualified_name",Es),
    		           La = glast("refac_util:do_add_range, qualified_name",Es),
		           {S1, _E1}= get_range(Hd),
		           {_S2, E2} = get_range(La),
  		           refac_syntax:add_ann({range, {S1, E2}}, Node);    
	 query_expr ->  B = refac_syntax:query_expr_body(Node),
			{S, E} = get_range(B),
			refac_syntax:add_ann({range, {S,E}}, Node);
         record_field -> Name = refac_syntax:record_field_name(Node),
                         {S1, E1} = get_range(Name),
                         Value = refac_syntax:record_field_value(Node),
                        case Value of 
			    none -> refac_syntax:add_ann({range, {S1, E1}}, Node);
			    _    -> {_S2, E2} = get_range(Value),
				    refac_syntax:add_ann({range, {S1, E2}}, Node)
			end;
	 record_expr -> Arg  = refac_syntax:record_expr_argument(Node),
                        Type = refac_syntax:record_expr_type(Node),
                        Fields = refac_syntax:record_expr_fields(Node),
                        {S1, E1} = case Arg of 
				       none -> get_range(Type);
				       _    -> get_range(Arg)
				   end,
                        case Fields of 
			    [] ->
				 E11 = extend_backwards(Toks, E1, '}'),
				 refac_syntax:add_ann({range, {S1,E11}}, Node);
			    _  -> {_S2, E2} = get_range(glast("refac_util:do_add_range,record_expr", Fields)),
				   E21 = extend_backwards(Toks, E2, '}'),
                                  refac_syntax:add_ann({range, {S1, E21}}, Node)
			end;
	 record_access -> Arg = refac_syntax:record_access_argument(Node),
			  Field = refac_syntax:record_access_field(Node),
			  {S1, _E1} = get_range(Arg),
			  {_S2, E2} = get_range(Field),
                          refac_syntax:add_ann({range, {S1, E2}}, Node);
	 record_index_expr -> Type = refac_syntax:record_index_expr_type(Node),
			      Field = refac_syntax:record_index_expr_field(Node),
			      {S1, _E1}= get_range(Type),
			      {_S2, E2} = get_range(Field),
			      refac_syntax:add_ann({range, {S1,E2}}, Node);
         comment ->  T = refac_syntax:comment_text(Node),
                     Lines = length(T), 
                    refac_syntax:add_ann({range, {{L,C}, 
			 {L+Lines-1,length(glast("refac_util:do_add_range,comment",T))}}}, Node);
	 macro -> Name = refac_syntax:macro_name(Node),
		  Args = refac_syntax:macro_arguments(Node),
		  {S1, E1} = get_range(Name),
		  case Args of  
		      none ->
			  refac_syntax:add_ann({range, {S1, E1}}, Node);
		      Ls -> La = glast("refac_util:do_add_range,macor",Ls),
		            {_S2, E2} = get_range(La),
  		            refac_syntax:add_ann({range, {S1, E2}}, Node)
		  end;
	 size_qualifier ->
	     Body = refac_syntax:size_qualifier_body(Node),
	     Arg  = refac_syntax:size_qualifier_argument(Node),
	     {S1, _E1} = get_range(Body),
	     {_S2, E2} = get_range(Arg),
	     refac_syntax:add_ann({range, {S1, E2}}, Node);	     
	_  ->io:format("Unhandled syntax category:\n~p\n", [refac_syntax:type(Node)]),
	     %%io:format("Node:\n~p\n", [Node]),
             Node
     end.
%% TOAdd:
%%       {bin_element, _, _, _, _} -> binary_field;
%%       {rule, _, _, _, _} -> rule;
%%       {'try', _, _, _, _, _} -> try_expr;




%% =====================================================================
%% add_category(Node::syntaxTree()) -> syntaxTree()
%%       
%% The purpose of the function is to distinguish patterns from expressions.
%% =====================================================================
add_category(Node) ->
    case refac_syntax:type(Node) of
      form_list -> Es = refac_syntax:form_list_elements(Node),
		   Es1 = lists:map((fun(E) -> add_category(E) end), Es),
		   Node1= refac_syntax:copy_attrs(Node, refac_syntax:form_list(Es1)),
		   refac_syntax:add_ann({category, form_list}, Node1);
      attribute -> add_category(Node , attribute);
      function ->  add_category(Node, function);
      rule -> add_category(Node, rule);
      error_marker -> add_category(Node, error_marker);
      warning_marker ->add_category(Node,warning_marker);
      eof_marker -> add_category(Node, eof_marker);
      comment -> add_category(Node, comment);
      macro   -> add_category(Node, macro);
      _ -> add_category(Node, unknown)
    end.

add_category(Node, C) -> stop_tdTP(fun do_add_category/2, Node, C).

do_add_category(Node, C) ->
     if is_list(Node) -> {lists:map(fun(E) -> add_category(E, C) end, Node), true};
	true -> case refac_syntax:type(Node) of
		 clause -> B = refac_syntax:clause_body(Node),
			   P = refac_syntax:clause_patterns(Node),
			   G = refac_syntax:clause_guard(Node),
			   B1 = add_category(B, expression),
			   P1 = add_category(P, pattern),
			   G1 = case G of 
				    none -> none; 
				    _   -> add_category(G, guard_expression)
				end,
			   Node1 = refac_syntax:copy_attrs(Node, refac_syntax:clause(P1, G1, B1)),
			   {refac_syntax:add_ann({category, clause}, Node1), true};
		 match_expr -> P = refac_syntax:match_expr_pattern(Node),
			       B = refac_syntax:match_expr_body(Node),
			       P1 = add_category(P, pattern),
			       B1 = add_category(B, expression),
		 	       Node1 = refac_syntax:copy_attrs(Node, refac_syntax:match_expr(P1, B1)),
			       {refac_syntax:add_ann({category, C}, Node1), true};
		 operator -> {refac_syntax:add_ann({category, operator}, Node), true}; %% added to fix bug 13/09/2008.
                 arity_qualifier ->  Fun = add_category(refac_syntax:arity_qualifier_body(Node), arity_qualifier),
				     A =  add_category(refac_syntax:arity_qualifier_argument(Node), arity_qualifier),
                                     Node1 = refac_syntax:arity_qualifier(Fun, A),
                                     {refac_syntax:add_ann({category, arity_qualifier}, Node1), true};
		 macro  -> Name = refac_syntax:macro_name(Node),
			   Args = refac_syntax:macro_arguments(Node),
			   Name1 = add_category(Name, macro_name),
			   Args1 = case Args of 
				       none -> none;
				       _ -> add_category(Args, expresssion) %% should 'expression' by 'macro_args'?
				   end,
			   Node1 = refac_syntax:copy_attrs(Node, refac_syntax:macro(Name1, Args1)),
			   {refac_syntax:add_ann({category, macro}, Node1), true};
		 attribute ->case refac_syntax:atom_value(refac_syntax:attribute_name(Node)) of 
				 define -> 
				     Name = refac_syntax:attribute_name(Node),
 				     Args = refac_syntax:attribute_arguments(Node),
 				     MacroHead = ghead("Refac_util:do_add_category:MacroHead",Args),
				     MacroBody = ghead("Refac_util:do_add_category:MacroBody",tl(Args)),
 				     MacroHead1 = 
					 case refac_syntax:type(MacroHead) of 
					     application -> 
						 Operator = add_category(
							      refac_syntax:application_operator(MacroHead),macro_name),
						 Arguments = add_category(
							       refac_syntax:application_arguments(MacroHead),attribute),
						 refac_syntax:copy_attrs(MacroHead, 
									 refac_syntax:application(Operator, Arguments));
					     _  -> add_category(MacroHead, macro_name)
					 end,
 				     MacroBody1 = add_category(MacroBody, attribute),
 				     Node1 = refac_syntax:copy_attrs(Node, refac_syntax:attribute(Name, 
 												  [MacroHead1, MacroBody1])),
 				     {refac_syntax:add_ann({category, attribute}, Node1), true};
				 _ -> {refac_syntax:add_ann({category, C}, Node), false}
			     end;
                 %% TO ADD: other cases such as fields. Refer to the Erlang Specification.
		 _       -> {refac_syntax:add_ann({category,C}, Node), false}
	     end
     end.

%% Adjust the locations of F and A in an implicit function application (fun F/A)
%% to their actual occurrence locations. Originally, both of their locations refer
%% to that of the keyword 'fun'.

%% Qn: Any other cases in need of location adjustment?

adjust_locations(FName, AST) ->
    {ok, Toks} = refac_epp:scan_file(FName, [],[]),
    F = fun(T) -> case refac_syntax:type(T) of
		    implicit_fun -> 
			Pos = refac_syntax:get_pos(T),
			Name = refac_syntax:implicit_fun_name(T),
			case refac_syntax:type(Name) of 
			    arity_qualifier ->
				Fun = refac_syntax:arity_qualifier_body(Name),
				A = refac_syntax:arity_qualifier_argument(Name),
				case {refac_syntax:type(Fun), refac_syntax:type(A)} of 
				    {atom, integer} -> 
					Toks1 = lists:dropwhile(fun(B) ->element(2, B) =/= Pos end, Toks),
					Fun1 = refac_syntax:atom_value(Fun),
					Toks2 = lists:dropwhile(fun(B)-> 
									case B of {atom, _, Fun1} -> false;
									    _ -> true
									end
								end, Toks1),				 
					P = element(2, ghead("refac_util: adjust_locations,P",Toks2)),
					Fun2 = refac_syntax:set_pos(Fun, P),
					Toks3 = lists:dropwhile(fun(B) ->
									case B of {integer, _, _} ->false;
									    _ -> true
									end
								end, Toks2),
					A2 = refac_syntax:set_pos(A, element(2, 
								     ghead("refac_util:adjust_locations:A2",Toks3))),
					refac_syntax:copy_attrs(T, refac_syntax:implicit_fun(
						 refac_syntax:set_pos(refac_syntax:copy_attrs(Name,
								  refac_syntax:arity_qualifier(Fun2, A2)), P)));
				    _ -> T
				end;
			    _ -> T
			end;
		    _ -> T
		end
	end,
    refac_syntax_lib:map(F, AST).


%% =====================================================================
%% update_var_define_locations(Node::syntaxTree()) -> syntaxTree()
%%       
%% Update the defining locations of those binding occurrences which are 
%% associated with more than one binding occrrence.
%% =====================================================================
update_var_define_locations(Node) ->
   F1= fun (T, S) -> case refac_syntax:type(T) of
			  variable ->
			      R = lists:keysearch(def, 1, refac_syntax:get_ann(T)),
			      case R of 
				  {value, {def, P}} -> S++[P];
				  _ -> S
			      end;
			  _ -> S
		     end
	end,
    DefineLocs = lists:usort(refac_syntax_lib:fold(F1, [], Node)),
    F = fun (T) ->
		case refac_syntax:type(T) of
		  variable ->
		      case lists:keysearch(def,1, refac_syntax:get_ann(T)) of 
			  {value, {def, Define}} -> Defs = lists:merge([V1
									|| V1 <- DefineLocs,
									   ordsets:intersection(ordsets:from_list(V1), 
												ordsets:from_list(Define))/=[]]),
						    update_ann(T, {def, lists:usort(Defs)});
			  _ -> T
		      end;
		    _ -> T
		end
	end,
    refac_syntax_lib:map(F, Node).

add_fun_define_locations(Node,Info) ->  %% DOSE INFO CONTAIN ANY LOCATION INFO? ANSWER: no.
    ModName = case lists:keysearch(module,1,Info) of
		  {value,{module, ModName1}} ->  ModName1;
		  _ -> ?DEFAULT_MODULE
	      end,
    Funs = fun (T, S) ->
		case refac_syntax:type(T) of
		  function ->
			ordsets:add_element({ModName, refac_syntax:data(refac_syntax:function_name(T)),
					     refac_syntax:function_arity(T),
					     refac_syntax:get_pos(T)},
					    S);
		    _ -> S
		end
	   end,
    Defined_Funs = refac_syntax_lib:fold(Funs, ordsets:new(), Node),
    Imps = case lists:keysearch(imports, 1, Info) of 
	       {value, {imports, I}} -> 
		   lists:concat([lists:map(fun({F,A}) -> {M1, F,A ,?DEFAULT_LOC} end ,Fs)||{M1, Fs} <- I]);
	       _ -> []
	   end,
    Inscope_Funs = Imps ++ Defined_Funs,
    Define_Mod_Loc = fun (Name, Arity) ->
			     Fs =ordsets:filter(fun ({_M, F, A, _Pos}) -> (F == Name) and (Arity == A) end, Inscope_Funs),
			     case Fs of
			       [] -> {erlang, ?DEFAULT_LOC};   %% is this correct? what about the function is not a BIF?
			       [{M, _, _, Pos} | _] -> {M,Pos}
			     end
		     end,
    F1 = fun (T) ->
		 case refac_syntax:type(T) of
		   function ->
		       Name = refac_syntax:function_name(T),
                       Fun_Name = refac_syntax:atom_value(Name),
                       Arity = refac_syntax:function_arity(T),
		       Pos = refac_syntax:get_pos(T),
		       T2 = [update_ann(C,{fun_def, {ModName,Fun_Name, Arity,refac_syntax:get_pos(C),Pos}})
			     || C <- refac_syntax:function_clauses(T)],
		       Name1 = update_ann(Name, {fun_def, {ModName,Fun_Name, Arity,Pos, Pos}}),
		       T3 = refac_syntax:copy_pos(T, refac_syntax:copy_attrs(T, refac_syntax:function(Name1, T2))),
		       update_ann(T3, {fun_def, {ModName,Fun_Name,Arity,Pos, Pos}});
		   application ->
		       Operator = refac_syntax:application_operator(T),
		       Arguments =refac_syntax:application_arguments(T),
		       case refac_syntax:type(Operator) of
			 atom ->
			       Op = refac_syntax:atom_value(Operator),
			       Arity = length(Arguments),
			       {DefMod, DefLoc} = Define_Mod_Loc(Op, Arity),
			       Operator1 = update_ann(Operator, 
                                           {fun_def, {DefMod,Op,Arity, refac_syntax:get_pos(Operator),DefLoc}}),
			       refac_syntax:copy_pos(T, refac_syntax:copy_attrs(T, 
									refac_syntax:application(Operator1, Arguments)));
		 	 module_qualifier ->
			       Mod = refac_syntax:module_qualifier_argument(Operator),
			       Fun = refac_syntax:module_qualifier_body(Operator),
			       case {refac_syntax:type(Mod), refac_syntax:type(Fun)} of 
				     {atom, atom} ->
				       M = refac_syntax:atom_value(Mod),
				       Fun_Name = refac_syntax:atom_value(Fun),
				       Arity = length(Arguments),
				       DefLoc = if M==ModName -> {_ModName, DefLoc1} = Define_Mod_Loc(Fun_Name, Arity),
								  DefLoc1;
						   true -> ?DEFAULT_LOC
						end,
				       Operator1 = refac_syntax:copy_attrs(Operator, 
									   refac_syntax:module_qualifier(Mod, Fun)),
                                       Operator2 = update_ann(Operator1, {fun_def, {M, Fun_Name, Arity, 
									      refac_syntax:get_pos(T),DefLoc}}),
				       refac_syntax:copy_attrs(T, refac_syntax:application(Operator2, Arguments));
				   _ -> T
			       end;
			 _ -> T
		       end;
		     arity_qualifier ->
			Fun = refac_syntax:arity_qualifier_body(T),
			A = refac_syntax:arity_qualifier_argument(T),
                        FunName = refac_syntax:atom_value(Fun), 
			Arity = refac_syntax:integer_value(A),
			{DefMod, DefLoc}= Define_Mod_Loc(FunName, Arity),
			Fun1=update_ann(Fun,{fun_def, {DefMod, FunName, Arity,refac_syntax:get_pos(Fun),DefLoc}}),
			update_ann(refac_syntax:copy_attrs(T, refac_syntax:arity_qualifier(Fun1, A)),
					   {fun_def, {DefMod,FunName,Arity,refac_syntax:get_pos(Fun),DefLoc}});
		     _ -> T
		 end
	 end,
    refac_syntax_lib:map(F1, Node).

%% =====================================================================
%% @spec envs_bounds_frees(Node::syntaxTree())-> {value, [{Key, Vars}]}
%%    
%%    Key = env | bound | free
%% @doc Return the input environment of the subtree, the variables that are 
%% bound as well as the variables that are free in the subtree.

envs_bounds_frees(Node) ->
      F = fun (T, B) ->
                As = refac_syntax:get_ann(T),
                EnVars = case lists:keysearch(env,1,As) of
			     {value, {env, EnVars1}} ->
				 EnVars1;
			     _ -> []
			 end,
                BdVars = case lists:keysearch(bound,1,As) of
			     {value, {bound, BdVars1}} ->
				 BdVars1;
			     _ -> []
			 end,
	        FrVars = case lists:keysearch(free,1, As) of 
			     {value, {free, FrVars1}} ->
				 FrVars1;
			     _ -> []
			 end,
	       case (EnVars==[]) and (BdVars==[]) and (FrVars==[]) of 
		   true -> B;
		   _ -> [{{env, EnVars},{bound, BdVars},{free, FrVars}}|B]
	       end
	end,
    lists:usort(refac_syntax_lib:fold(F, [], Node)).	

%% =====================================================================
%% @spec write_refactored_files([FileName:: filename(), AST::syntaxTree()])-> ok
%%    
%% @doc Pretty-print the abstract syntax trees to a files, and add the previous version to history for undo purpose.

write_refactored_files(Files) ->
    F = fun({{File1, File2}, AST}) ->
		if File1 /= File2 ->
			file:delete(File1),
			file:write_file(File2, list_to_binary(refac_prettypr:format(AST)));
		   true -> file:write_file(File2, list_to_binary(refac_prettypr:format(AST)))
		end
	end,
    Files1 = lists:map((fun({{OldFileName, NewFileName}, _}) -> 
				{ok, Bin} = file:read_file(OldFileName), 
				{{OldFileName, NewFileName}, Bin} end), Files),
    case erlang:whereis(refactor_undo) of 
	undefined ->
	    io:format("\nWARNING: the UNDO process is not working, please restart the refactorer!\n");
	_ ->refactor_undo ! {add, Files1}  
    end,
    lists:map(F, Files).

%% =====================================================================
%% @spec is_fun_name(Name:: string())-> Bool
%%    
%% @doc Return true if a name is lexically a legal function name.
is_fun_name(Name) ->
    case Name of
      [H |T] -> is_lower(H) and is_var_name_tail(T);
      [] -> false
    end.

%% =====================================================================
%% @spec is_var_name(Name:: string())-> Bool
%%    
%% @doc Return true if a string is lexically a legal variable name.
is_var_name(Name) ->
    case Name of
      [H | T] -> (is_upper(H) or (H==95)) and is_var_name_tail(T);
      [] -> false
    end.

is_var_name_tail(Name) ->
    case Name of
      [H | T] ->
	  (is_upper(H) or is_lower(H) or is_digit(H) or (H == 64) or (H == 95)) and
	    is_var_name_tail(T);
      [] -> true
    end.

is_upper(L) -> (L >= 65) and (90 >= L).

is_lower(L) -> (L >= 97) and (122 >= L). 

is_digit(L) -> (L >= 48) and (57 >= L).

%% =====================================================================
%% @spec is_expr(Node:: syntaxTree())-> bool()
%%    
%% @doc Return true if the subtree represents an expression.
is_expr(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(category,1, As) of 
	{value, {category, C}} ->case C of
				     expression -> true;
				     guard_expression -> true;
				     _  -> false
				 end;
	_ -> false
    end. 

is_pattern(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(category,1, As) of 
	{value, {category, C}} ->case C of
				     pattern -> true;
				     _  -> false
				 end;
	_ -> false
    end. 

%% =====================================================================
%% @spec expr_to_fun(Tree::syntaxTree(), Exp::syntaxTree())-> {ok, syntaxTree()} | {error, term()}
%%    
%% @doc Return the the syntaxTree of the function that contains the expression. 
%% TODO: CHANGE THE RETURN TYPE OF THIS FUNCTION.
expr_to_fun(Tree, Exp) ->
    {Start, End} = get_range(Exp),
    {S, E} = get_range(Tree),
    if (S <Start) and (E >= End) ->
	     case refac_syntax:type(Tree) of 
		 function ->
		     [Tree];
		 _  -> Ts = refac_syntax:subtrees(Tree),
		       R0 = [[expr_to_fun(T, Exp) || T <-G] || G <- Ts],
		      lists:flatten(R0)
	     end;
       true -> []
    end.



%% =====================================================================
%% @spec get_var_exports(Node::syntaxTree())-> {[Vars]}
%%    
%% @doc Return the variables that are exported by the subtree. 

get_var_exports(Node) ->
    get_var_exports_1(refac_syntax:get_ann(Node)).

get_var_exports_1([{bound, B} | _Bs]) -> B;
get_var_exports_1([_ | Bs]) -> get_var_exports_1(Bs);
get_var_exports_1([]) -> [].

%% =====================================================================
%% @spec get_free_vars(Node::syntaxTree())-> {[Vars]}
%%    
%% @doc Return the variables that are free in the subtree.
get_free_vars(Node) ->
    get_free_vars_1(refac_syntax:get_ann(Node)).

get_free_vars_1([{free, B} | _Bs]) -> B;
get_free_vars_1([_ | Bs]) -> get_free_vars_1(Bs);
get_free_vars_1([]) -> [].

%% =====================================================================
%% @spec get_bound_vars(Node::syntaxTree())-> {[Vars]}
%%    
%% @doc Return the variables that are bound in the subtree. 
get_bound_vars(Node) ->
    get_bound_vars_1(refac_syntax:get_ann(Node)).

get_bound_vars_1([{bound, B} | _Bs]) -> B;
get_bound_vars_1([_ | Bs]) -> get_bound_vars_1(Bs);
get_bound_vars_1([]) -> [].

%% =====================================================================
%% @spec update_ann(Node::syntaxTree(), {Key, term()}) -> syntaxTree()
%%       
%% @doc Update a specific annotation of the Node with the given one.
update_ann(Tree, {Key, Val}) ->
    As0 = refac_syntax:get_ann(Tree),
    As1 = case lists:keysearch(Key, 1, As0) of 
	      {value, _} -> lists:keyreplace(Key, 1, As0, {Key, Val});
	      _ -> As0++[{Key, Val}]
	  end,    
    refac_syntax:set_ann(Tree, As1).

%% =====================================================================
%% @spec reset_attrs(Node::syntaxTree()) -> syntaxTree()
%%       
%% @doc Reset all the annotations in the subtree to the default (empty) annotation.

reset_attrs(Node) ->
    refac_util:full_buTP(fun (T) -> refac_syntax:set_attrs(T, #attr{}) end, Node).

%% =====================================================================
%% @spec inscope_funs(ModuleInfo) -> [{ModuleName, FunctionName, Arity}]
%%       
%%        ModuleName = atom()
%%        FunctionName = atom()
%%        Arity        = integer()
%%
%% @doc Return the functions that are imported/defined by a module.

%% TODO: THINK ABOUT THIS FUNCTION AGAIN. The current definition assumes 
%% that the correct module information is given.
inscope_funs(ModuleInfo) ->
    case lists:keysearch(module,1, ModuleInfo) of 
	{value,{module, M}}->
	Imps = case lists:keysearch(imports, 1, ModuleInfo) of 
		   {value, {imports, I}} -> 
		       lists:concat([lists:map(fun({F,A}) -> {M1, F,A} end ,Fs)||{M1, Fs} <- I]);
		   _ -> []
	       end,
	Funs = case lists:keysearch(functions, 1, ModuleInfo) of 
		   {value, {functions, Fs}} -> lists:map(fun({F,A}) -> {M,F,A} end , Fs);
		   _ -> []
	       end,
	Imps ++ Funs;
	_  -> []
    end.



%% =====================================================================
%% @spec get_range(Node::syntaxTree())-> {Pos, Pos}
%%    
%% @doc Return the start and end locations of the subtree in the program source. 

%%TODO: CHECK THE RETURN TYPE.
get_range(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(range, 1, As) of
      {value, {range, {S, E}}} ->
	    {S,E};
      _ ->  %%io:format("range_not_found:\n~p\n", [Node]),
	    {{?DEFAULT_LOC, ?DEFAULT_LOC}} %% erlang:fault(range_not_found)
    end.


%%===============================================================================
%% @spec is_exported({FunctionName, Arity},ModuleInfo} -> Bool
%%
%% @doc Return true if the function is exported by its defining module.

is_exported({Fun, Arity}, ModInfo) ->
     case lists:keysearch(exports, 1, ModInfo) of 
	{value,{exports, ExportList}} -> R = lists:member({Fun, Arity}, ExportList),
					 if R -> R;
					    true ->case lists:keysearch(attributes,1,ModInfo) of 
						       {value, {attributes, Attrs}} -> 
							   lists:member({compile, export_all}, Attrs);
						       false -> false
						   end 
					 end;	
	false -> case lists:keysearch(attributes,1,ModInfo) of 
		     {value, {attributes, Attrs}} -> 
			 lists:member({compile, export_all}, Attrs);
		     false -> false
		 end
    end.

get_client_files(File, SearchPaths) ->
    ValidSearchPaths= lists:all(fun(X)->filelib:is_dir(X) end, SearchPaths),
    case ValidSearchPaths of 
	true -> ok;
	false -> exit("One of the directories sepecified in the search paths does not exist, please check the customization!")
    end,
    ModuleGraphFile = filename:join([filename:dirname(File), "modulegraph"]),
    File1 = filename:absname(normalise_file_name(File)),
    Dir = filename:dirname(File1),
    WranglerOptions = #options{},   %% TODO: This should be changed to a globe one.
    ModuleGraph= refac_module_graph:module_graph(lists:usort([Dir|SearchPaths]), ModuleGraphFile, WranglerOptions),
    ClientFiles =case lists:keysearch(File1, 1, ModuleGraph) of 
		       {value, {_, Clients}} ->
			   lists:delete(File1, Clients);
		       _ -> 
			 []
		 end,
    
    case ClientFiles of 
	[] -> io:format("\nWARNING: this module does not have any client modules, please check the search paths to ensure that this is correct!\n");
	_ -> ok
    end,
    HeaderFiles =expand_files(SearchPaths, ".hrl"),
    ClientFiles ++ HeaderFiles.
    


normalise_file_name(Filename) ->
     filename:join(filename:split(Filename)).
%% =====================================================================


collect_var_mod_qualifiers(FileName) ->
       case refac_util:parse_annotate_file(FileName,1) of
	   {ok, {AnnAST, Info}} -> 
	       {ok, ModName} = get_module_name(Info),
	       collect_var_mod_qualifiers_1(AnnAST, ModName);
	   {error, _Reason} -> {error, "Error with parsing/annotating file "++ FileName}
       end.
collect_var_mod_qualifiers_1(Tree, ModName) ->
     F= fun(T, S) ->
 		   case refac_syntax:type(T) of 
 		       function ->
			   Arity = refac_syntax:function_arity(T),
			   S++ contains_var_mod_qualifier(T, ModName,Arity );
		       _  -> S
 		   end
	    end,
    refac_syntax_lib:fold(F, [], Tree).
		   
contains_var_mod_qualifier(Node,ModName,Arity) ->    
    FunName = refac_syntax:atom_value(refac_syntax:function_name(Node)),
    Clauses =refac_syntax:function_clauses(Node),
    F2 = fun(T,S) -> 
		 case refac_syntax:type(T) of 
		     module_qualifier ->
			 Mod = refac_syntax:module_qualifier_argument(T),
		         case refac_syntax:type(Mod) of 
			     variable ->
				 case lists:keysearch(def, 1, refac_syntax:get_ann(Mod)) of 
				     {value, {def, _DefinePos}} -> 
					 S ++[{ModName, FunName, Arity}];
				               %% ,refac_syntax:variable_name(Mod), DefinePos}];
				     _ -> S
				 end;
			     atom -> S;
			     _  -> S ++ [{ModName, FunName, Arity}] %%{refac_syntax:atom_value
			 end;
		     _ -> S
		 end
	 end,
    F1 = fun(Clause) ->
		 As = refac_syntax:get_ann(Clause),
		 case lists:keysearch(bound,1, As) of 
		     {value, {bound, _BdVars}} -> 
			 refac_syntax_lib:fold(F2, [], Clause);
		     _  -> []
		 end
	 end,
   lists:flatmap(F1, Clauses).

get_module_name(ModInfo) ->				      
    case lists:keysearch(module, 1, ModInfo) of
	{value, {module, ModName}} -> {ok, ModName};
	false ->
	    {error, "Can not get the current module name."}
    end.

callback_funs(Behaviour) ->
    case Behaviour of  
	gen_server -> [{init, 1}, {handle_call,3}, {handle_cast,2},
		       {handle_info,2},{terminate, 2},{code_change,3}];
	gen_event ->[{init, 1}, {handle_event,2},{handle_call,2},
		     {handle_info,2},{terminate, 2}, {code_change,3}];
	gen_fsm  -> [{init,1},{handle_event,3},{handle_sync_event,4},
		     {handle_info,3},{terminate, 3},{code_change, 4}];
	supervisor -> [{init,1}];
	_  -> []
    end.
    
		     
to_upper(Str) ->
    to_upper(Str, []).

to_upper([C|Cs], Acc) when C >= $a, C =< $z ->
    to_upper(Cs, [C-($a-$A)| Acc]);
to_upper([C|Cs], Acc) ->
    to_upper(Cs, [C | Acc]);
to_upper([], Acc) ->
    lists:reverse(Acc).

to_lower(Str) ->
    to_lower(Str, []).
to_lower([C|Cs], Acc) when C >= $A, C =< $Z ->
    to_lower(Cs, [C+($a-$A)| Acc]);
to_lower([C|Cs], Acc) ->
    to_lower(Cs, [C| Acc]);
to_lower([], Acc) ->
    lists:reverse(Acc).
	   
	   
%%=================================================================	
from_dets(Name, Dets) when is_atom(Name) ->
  Plt = ets:new(Name, [set, public]),
  {ok, D} = dets:open_file(Dets, [{access, read}]),
  true = ets:from_dets(Plt, D),
  ok = dets:close(D),
  Plt.

to_dets(Plt, Dets) ->
  file:delete(Dets),
  MinSize = ets:info(Plt, size),
  {ok, Dets} = dets:open_file(Dets, [{min_no_slots, MinSize}]),
  ok = dets:from_ets(Dets, Plt),
  ok = dets:sync(Dets),
  ok = dets:close(Dets).

lookup(Plt, {M, F, A}) ->
  case ets:lookup(Plt, {M, F, A}) of
    [] -> none;
    [{_MFA, S}] -> {value, S}
  end.

has_side_effect(Node,Side_Effect_Table) ->
    case refac_syntax:type(Node) of 
	receive_expr -> true;
	infix_expr -> Op = refac_syntax:operator_literal(refac_syntax:infix_expr_operator(Node)),
		      Op == "!";
	application -> 
 	    Operator = refac_syntax:application_operator(Node),
 	    Arity = length(refac_syntax:application_arguments(Node)),
 	    case refac_syntax:type(Operator) of 
		atom ->
		    Op = refac_syntax:atom_value(Operator),
		    {value, {fun_def, {M, _N, _A, _P1, _P}}} 
			= lists:keysearch(fun_def, 1, refac_syntax:get_ann(Operator)),
		    case lookup(Side_Effect_Table, {M, Op, Arity}) of 
			{value, S} -> S;
			_ -> unknown
		    end;
		module_qualifier ->
		    Mod = refac_syntax:module_qualifier_argument(Operator),
		    Body = refac_syntax:module_qualifier_body(Operator),
		    case {refac_syntax:type(Mod), refac_syntax:type(Body)} of 
			{atom, atom} ->
			    M = refac_syntax:atom_value(Mod),
			    Op = refac_syntax:atom_value(Body),
			    case lookup(Side_Effect_Table, {M, Op, Arity}) of 
				{value, S} -> S;
				_ -> unknown 
			    end;
			_ -> true %% QUESTION: SHOULD THIS BE FALSE OR TRUE?
		    end;
		_ -> true   %% IS THIS CORRECT?
	    end;
	arity_qualifier ->
	    Fun = refac_syntax:arity_qualifier_body(Node),
	    A = refac_syntax:arity_qualifier_argument(Node),
	    case {refac_syntax:type(Fun), refac_syntax:type(A)} of 
		{atom, integer} ->
		    FunName = refac_syntax:atom_value(Fun),
		    Arity = refac_syntax:integer_value(A),
		    {value, {fun_def, {M, _N, _A, _P1, _P}}} = 
			lists:keysearch(fun_def, 1, refac_syntax:get_ann(Node)),
		    case lookup(Side_Effect_Table, {M, FunName, Arity}) of
			{value, S} -> S;
			_ -> unknown
		    end;		
		_ ->  true  %% IS THIS CORRECT?
	    end;
 	 _ -> case refac_syntax:subtrees(Node) of 
 		  [] -> false;
 		  Ts  -> Res =lists:flatten([[has_side_effect(T, Side_Effect_Table) || T <-G] || G <- Ts]),
			 case lists:member(true, Res) of
			     true -> true;
			     false -> case lists:member(unknown, Res) of 
					  true -> unknown;
					  _    -> false
				      end
			 end			 
 	      end
    end.

expand_files(Files, Ext) ->
  expand_files(Files, Ext, []).

expand_files([File|Left],Ext, Acc) ->
  case filelib:is_dir(File) of
    true ->
      {ok, List} = file:list_dir(File),
      NewFiles = [filename:join(File, X)
		  || X <- List, filelib:is_file(filename:join(File,X)), filename:extension(X)==Ext],
      NewDirs = [filename:join(File, X) || X <- List, filelib:is_dir(filename:join(File, X))],
      expand_files(NewDirs++Left, Ext, NewFiles++Acc);
    false -> case filelib:is_regular(File) of 
		 true -> case filename:extension(File) ==Ext of 
			     true -> expand_files(Left, Ext, [File|Acc]);
			     false -> expand_files(Left, Ext, [File])
			 end;
		 _  -> expand_files(Left, Ext, [File])
	     end
  end;
expand_files([], _Ext, Acc) ->
  ordsets:from_list(Acc).

get_modules_by_file(Files)->
  get_modules_by_file(Files,[]).

get_modules_by_file([File|Left], Acc)->
  BaseName = filename:basename(File, ".erl"),
  Dir = filename:dirname(File),
  get_modules_by_file(Left, [{BaseName, Dir}|Acc]);
get_modules_by_file([], Acc) ->
  lists:reverse(Acc).


%% @spec has_side_effect(Node:: syntaxTree)-> bool()
%%    
%% @doc Return true if the subtree has side effect.

has_side_effect(Node,Info, FileName) ->
    File = filename:join(?WRANGLER_DIR, "plt/side_effect_plt"),
    io:format("Wrangler_dir:\n~p\n", [?WRANGLER_DIR]),
    io:format("File:\n~p\n", [File]),
    Plt = from_dets(side_effect_plt, File),
    Res = has_side_effect(Node,Plt),
    case Res of 
	true -> true;
	false -> false;
	unknown -> Plt1 = build_sideeffect_tab(Node, Info, FileName),
		   Res1 = has_side_effect(Node, Plt1),
		   case Res1 of 
		       true -> true;
		       false -> false;
		       unknown -> true  %% TODO: THINK ABOUT THIS AGAIN.
		   end
    end.
			    

build_sideeffect_tab(_Node, _Info, FileName) ->   %% TODO: REMOVE UNUSED PARAMETERS!!!
     File = filename:join(?WRANGLER_DIR, "plt/side_effect_plt"),
     Plt = from_dets(side_effect_plt, File), 
     Dir = filename:dirname(FileName),
     {Sccs, _E} = build_call_graph([Dir]), %%(Node, Info, FileName),
     side_effect_tab1(Sccs, Plt).


build_sideeffect_tab(Dirs) ->
    Plt = ets:new(side_effect_table, [set,public]),
    true = ets:insert(Plt, side_effect_table()),
    {Sccs, _E} =build_call_graph(Dirs),
    side_effect_tab1(Sccs,Plt),
    File = filename:join(?WRANGLER_DIR, "plt/side_effect_plt"),
    to_dets(Plt, File).
   
side_effect_tab1([Scc|Left], Side_Effect_Table) ->
     R= side_effect_scc(Scc, Side_Effect_Table),
     true = ets:insert(Side_Effect_Table, [{{Mod, Fun, Arg}, R} ||
					      {{Mod, Fun, Arg}, _F}<-Scc]),
     side_effect_tab1(Left, Side_Effect_Table);
side_effect_tab1([], Side_Effect_Table) ->
     Side_Effect_Table.

side_effect_scc([{{_M, _F, _As}, {_File,Def}}|Left],Side_Effect_Table) ->
    case has_side_effect(Def,Side_Effect_Table) of
        true -> true;
        _ -> side_effect_scc(Left, Side_Effect_Table)  %% Check this again: true, false. unknown.
     end;
side_effect_scc([], _Side_Effect_Table) -> false.
	    
side_effect_table() ->
   [{{erlang, abs, 1}, false},
    {{erlang, append_element, 2}, false},
    {{erlang, atom_to_list, 1}, false},
    {{erlang, binary_to_list,1}, false},
    {{erlang, binary_to_list,3}, false},
    {{erlang, binary_to_term, 1}, false},
    {{erlang, bump_reductions, 1}, false},
    {{erlang, cancel_timer, 1}, true},
    {{erlang, check_process_code,1}, false},
    {{erlang, concat_binary, 1}, false},
    {{erlang, data, 3}, false},
    {{erlang, delete_module, 1}, true},
    {{erlang, demonitor, 1}, false},
    {{erlang, disconnect_node, 1}, true},
    {{erlang, display, 1}, true},
    {{erlang, element, 2}, false},
    {{erlang, erase, 0}, true},
    {{erlang, erase, 1}, true},
    {{erlang, error, 1}, true},
    {{erlang, error, 2}, true},
    {{erlang, exit, 1}, true},
    {{erlang, exit, 2}, true},
    {{erlang, fault, 1}, true},
    {{erlang, fault, 2}, true},
    {{erlang, float, 1}, false},
    {{erlang, float_to_list, 1}, false},
    {{erlang, fun_info, 2}, false},
    {{erlang, fun_info,1}, false},
    {{erlang, fun_to_list, 1}, false},
    {{erlang, function_exported, 3},true},
    {{erlang, garbage_collect,1}, true},
    {{erlang, garbage_collect, 0}, true},
    {{erlang, get, 0}, true},
    {{erlang, get, 1}, true},
    {{erlang, get_cookie, 0}, true},
    {{erlang, get_keys, 1}, true},
    {{erlang, get_stacktrace, 0}, true},
    {{erlang, group_leader, 0}, true},
    {{erlang, group_leader, 2}, true},
    {{erlang, halt, 0}, true},
    {{erlang, halt, 1}, true},
    {{erlang, hash, 2}, false},
    {{erlang, hd, 1}, false},
    {{erlang, hibernate, 3}, true},
    {{erlang, info, 1}, true},
    {{erlang, integer_to_list, 1}, false},
    {{erlang, iolist_to_binary, 1}, false},
    {{erlang, iolist_size, 1},false},
    {{erlang, is_atom, 1}, false},
    {{erlang, is_binary, 1}, false},
    {{erlang, is_boolean, 1}, false},
    {{erlang, is_builtin, 3}, false},
    {{erlang, is_float, 1}, false},
    {{erlang, is_function, 1}, false},
    {{erlang, is_function, 2}, false},
    {{erlang, is_integer, 1}, false},
    {{erlang, is_list, 1}, false},
    {{erlang, is_number, 1}, false},
    {{erlang, is_pid, 1}, true},
    {{erlang, is_port, 1}, false},
    {{erlang, is_process_alive, 1}, true},
    {{erlang, is_record, 2}, false},
    {{erlang, is_record, 3}, false},
    {{erlang, is_reference, 1}, false},
    {{erlang, is_tuple, 1}, false},
    {{erlang, length, 1}, false},
    {{erlang, link, 1}, true},
    {{erlang, list_to_atom, 1}, false},
    {{erlang, list_to_binary, 1}, false},
    {{erlang, list_to_existing_atom, 1},false},
    {{erlang, list_to_float, 1}, false},
    {{erlang, list_to_integer, 1}, false},
    {{erlang, list_to_integer, 2}, false},
    {{erlang, list_to_pid, 1}, false},
    {{erlang, list_to_tuple, 1}, false},
    {{erlang, load_module, 2}, true},
    {{erlang, loaded, 0}, true},
    {{erlang, localtime, 0}, true},
    {{erlang, localtime_to_universaltime, 1}, false},
    {{erlang, localtime_to_iniversaltime, 2}, false},
    {{erlang, make_ref, 0}, true},
    {{erlang, make_tuple, 2}, true},
    {{erlang, md5, 1}, false},
    {{erlang, md5_final, 1}, false},
    {{erlang, md5_init, 0, false},
    {{erlang, md5_update, 2}, false},
    {{erlang, memory, 0}, true},
    {{erlang, memory, 1}, true},
    {{erlang, module_loaded, 1}, true},
    {{erlang, monitor, 2}, true},
    {{erlang, monitor_node, 2}, true},
    {{erlang, node, 0}, true},
    {{erlang, node, 1}, true}, 
    {{erlang, nodes, 0}, true},
    {{erlang, nodes, 1}, true},
    {{erlang, now, 0}, true},
    {{erlang, open_port, 2}, true},
    {{erlang, phash, 2}, false},
    {{erlang, phash2, 2}, false},
    {{erlang, pid_to_list, 1}, true},
    {{erlang, port_close, 1}, true},
    {{erlang, port_command, 2}, true},
    {{erlang, port_connect, 2}, true},
    {{erlang, port_control, 3}, true},
    {{erlang, port_call, 3}, true},
    {{erlang, port_info, 1}, true},
    {{erlang, port_info, 2}, true}, 
    {{erlang, port_to_list, 1}, true},
    {{erlang, ports, 0}, true},
    {{erlang, pre_loaded, 0}, true},
    {{erlang, process_diaplay, 2}, true},
    {{erlang, process_flag, 2}, true},
    {{erlang, process_flag, 3}, true},
    {{erlang, process_info, 1}, true},
    {{erlang, process_info, 2}, true},
    {{erlang, processes, 0}, true},
    {{erlang, purge_module, 1}, true},
    {{erlang, put, 2}, true},
    {{erlang, raise, 3}, true},
    {{erlang, read_timer, 1}, true},
    {{erlang, ref_to_list, 1}, false},
    {{erlang, register, 2}, true},
    {{erlang, registered, 0}, true},
    {{erlang, resume_process, 1}, true},
    {{erlang, round, 1}, false},
    {{erlang, self, 0}, true},
    {{erlang, send, 2}, true},
    {{erlang, send, 3}, true},
    {{erlang, send_after, 3}, true},
    {{erlang, send_nosuspend, 2}, true},
    {{erlang, send_nosuspend, 3}, true},
    {{erlang, set_cookie, 2}, true},
    {{erlang, setelement, 3}, false},
    {{erlang, size, 1}, false},
    {{erlang, spawn, 1}, true},
    {{erlang, spawn, 2}, true},
    {{erlang, spawn, 3}, true},
    {{erlang, spawn, 4}, true},
    {{erlang, spawn_link, 1}, true},
    {{erlang, spawn_link, 2}, true},
    {{erlang, spawn_link, 3}, true},
    {{erlang, spawn_link, 4}, true},
    {{erlang, spawn_opt, 2}, true},
    {{erlang, spawn_opt, 3}, true},
    {{erlang, spawn_opt, 4}, true},
    {{erlang, spawn_opt, 5}, true},
    {{erlang, aplit_binary, 2}, false},
    {{erlang, start_timer, 3}, true},
    {{erlang, statistics, 1}, true},
    {{erlang, suspend_process, 1}, false},
    {{erlang, system_flag, 2}, true},
    {{erlang, system_info, 1}, true},
    {{erlang, system_monitor, 0}, true},
    {{erlang, system_monitor, 1}, true},
    {{erlang, system_monitor, 2}, true},
    {{erlang, term_to_binary, 1}, false},
    {{erlang, term_to_binary, 2}, false},
    {{erlang, throw, 1}, true},
    {{erlang, time, 1}, true},
    {{erlang, tl, 1}, false},
    {{erlang, trace, 1}}, true},
    {{erlang, trace_info, 2}, true},
    {{erlang, trace_pattern,2}, true},
    {{erlang, trace_pattern,3},true},
    {{erlang, trunc, 1}, false},
    {{erlang, unregister, 1}, false}, 
    {{erlang, unregister, 1}, true},
    {{erlang, tuple_to_list, 1}, false},
    {{erlang,universaltime, 1}, false},
    {{erlang, universaltime_to_localtime, 1}, false},
    {{erlang, unlink, 1}, true},
    {{erlang, whereis, 1}, true},
    {{erlang, yield, 1}, true}].
 

build_call_graph(Dir) ->
    Files = refac_util:expand_files(Dir, ".erl"),
    CallGraph=build_call_graph1(Files, []),
    #callgraph{scc_order=Sccs,external_calls=E}=refac_callgraph:construct(CallGraph),
    {Sccs, E}.

build_call_graph1([FileName|Left], Acc) ->
    case refac_util:parse_annotate_file(FileName,1) of 
	{ok, {AnnAST, Info}} ->
	    G1 = build_call_graph(AnnAST, Info, FileName),
	    Acc1= Acc++ G1,
	    build_call_graph1(Left, Acc1);
	{error, Reason} -> {error, Reason}
    end;
build_call_graph1([], Acc) ->
    Acc.
    
build_call_graph(Node, Info, FileName) ->
    {value,{module, ModName}} = lists:keysearch(module, 1, Info),
     Inscope_Funs = refac_util:inscope_funs(Info),  %% QUESTION: HOW ABOUT THE BUILT IN FUNCTIONS?
     F2 = fun(T, S) -> 
 		 case refac_syntax:type(T) of 
 		     application -> 
 			 Operator = refac_syntax:application_operator(T),
 			 Arity = length(refac_syntax:application_arguments(T)),
 			 case refac_syntax:type(Operator) of 
 			     atom ->
 				 Op = refac_syntax:atom_value(Operator),
				 R = lists:filter(fun({_M, F, A})-> (F ==Op) and (A == Arity) end, Inscope_Funs),
				 if R == [] -> ordsets:add_element({erlang, Op, Arity}, S);
				    true -> {M, Op, Arity} = hd(R),
					    ordsets:add_element({M, Op, Arity}, S)
				 end;
			     module_qualifier ->
				 Mod  = refac_syntax:module_qualifier_argument(Operator),
				 Body = refac_syntax:module_qualifier_body(Operator),
				 case {refac_syntax:type(Mod), refac_syntax:type(Body)} of 
				     {atom, atom} -> Mod1 = refac_syntax:atom_value(Mod),
						     Op = refac_syntax:atom_value(Body),
						     ordsets:add_element({Mod1, Op, Arity}, S);
				    _ ->  S 
				 end;
			     _ -> S
			 end;
		     arity_qualifier ->
			 Fun = refac_syntax:arity_qualifier_body(T),
			 A = refac_syntax:arity_qualifier_argument(T),
			 case {refac_syntax:type(Fun), refac_syntax:type(A)} of 
			     {atom, integer} ->
				 FunName = refac_syntax:atom_value(Fun),
				 Arity = refac_syntax:integer_value(A),
				 ordsets:add_element({ModName, FunName, Arity}, S);
			     _ -> S
			 end;
		    _  -> S
		 end
	  end,
     F1 = fun(T, S) ->
 		case refac_syntax:type(T) of 
 		    function -> FunName = refac_syntax:data(refac_syntax:function_name(T)),
 				Arity = refac_syntax:function_arity(T),
 				Caller = {{ModName, FunName, Arity},{FileName, T}},
 				CalledFuns =lists:usort(refac_syntax_lib:fold(F2, [], T)),
 				ordsets:add_element({Caller, CalledFuns}, S);
 		    _ -> S 
 		end
 	end,
    lists:usort(refac_syntax_lib:fold(F1, [], Node)).


try_evaluation(Expr) ->
    case catch erl_eval:exprs(Expr, []) of
      {value, V, _} -> {value, V};
      _ -> {error, "Error with evaluation"}
    end. 
    
 
token_val(T) ->
    case T of 
	{_, _, V} -> V;
	{V, _} -> V
    end.

token_loc(T) ->
      case T of 
	{_, L, _V} -> L;
	{_, L1} -> L1
      end.

extend_forwards (Toks, StartLoc, Val) ->
    Toks1 = lists:takewhile(fun(T) -> token_loc(T)  < StartLoc end, Toks),
    Toks2 = lists:dropwhile(fun(T) -> token_val(T) =/= Val end, lists:reverse(Toks1)),
    case Toks2 of
	[] -> StartLoc;
	_ ->token_loc(hd(Toks2))
    end.


extend_backwards(Toks, EndLoc, Val) ->
    Toks1 = lists:dropwhile(fun(T) -> token_loc(T) =< EndLoc end, Toks),
    Toks2= lists:dropwhile(fun(T) -> token_val(T) =/= Val end, Toks1),
    case Toks2 of 
	[] -> EndLoc;
	_ -> {Ln, Col} = token_loc(hd(Toks2)),
	     {Ln, Col+length(atom_to_list(Val))-1}
    end.
  


    



