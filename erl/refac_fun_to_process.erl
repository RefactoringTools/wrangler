%% =====================================================================
%% Refactoring: Rename a variable name.
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
%% @doc Register a process id, Pid say, with a name, RegName say, and replace
%% the uses of Pid ! Msg with  RegName ! Msg.
%% @end


%% TODO: 1. should trace back to the define location of the user-selected pid.
%% 

-module(refac_fun_to_process).

-export([fun_to_process/5, fun_to_process_eclipse/5]).

-include("../hrl/wrangler.hrl").


%% =====================================================================
%% @spec rename_var(FileName::filename(), Line::integer(), Col::integer(), NewName::string(),SearchPaths::[string()])-> term()
%%
-spec(fun_to_process/5::(filename(), integer(), integer(), string(), [dir()]) -> {ok, [filename()]} | {error, string()}).	     
fun_to_process(FName, Line, Col, ProcessName, SearchPaths) ->
    fun_to_process(FName, Line, Col, ProcessName, SearchPaths, emacs).


-spec(fun_to_process_eclipse/5::(filename(), integer(), integer(), string(), [dir()]) -> {ok, [{filename(), filename(), string()}]} | {error, string()}).
fun_to_process_eclipse(FName, Line, Col, ProcessName, SearchPaths) ->
    fun_to_process(FName, Line, Col, ProcessName, SearchPaths, eclipse).

fun_to_process(FName, Line, Col, ProcessName, SearchPaths, Editor) ->
    io:format("\n[CMD: fun_to_process, ~p, ~p, ~p, ~p,~p]\n",  [FName, Line, Col, ProcessName, SearchPaths]),
    case refac_util:is_fun_name(ProcessName) of
	true ->
	    case refac_util:parse_annotate_file(FName, true, SearchPaths) of
		{ok, {AnnAST, Info}} ->
		    {value, {module, ModName}} = lists:keysearch(module, 1, Info),
		    ProcessName1 = list_to_atom(ProcessName), 
		    case refac_util:pos_to_fun_name(AnnAST, {Line,Col}) of
			 {ok, {Mod, FunName, Arity, _, DefinePos}} ->
			    if Mod == ModName -> 
				    case pre_cond_check(FName, AnnAST, {Line, Col}, ModName,FunName,Arity, ProcessName1) of 
					ok ->AnnAST1 = do_fun_to_process(AnnAST, ModName, DefinePos, FunName, Arity, ProcessName1),
					     case refac_util:is_exported({FunName, Arity}, Info) of
						 true ->
						     io:format("\nChecking client modules in the following "
							       "search paths: \n~p\n",
							       [SearchPaths]),
						     ClientFiles = refac_util:get_client_files(FName, SearchPaths),
						     Results = fun_to_process_in_client_modules(ClientFiles, ModName, FunName, Arity, ProcessName1, SearchPaths),
						     case Editor of 
							 emacs ->
							     refac_util:write_refactored_files([{{FName, FName}, AnnAST1} | Results]),
							     ChangedClientFiles = lists:map(fun ({{F, _F}, _AST}) -> F end, Results),
							     ChangedFiles = [FName | ChangedClientFiles],
							     io:format("The following files have been changed "
								       "by this refactoring:\n~p\n",
								       [ChangedFiles]),
							     {ok, ChangedFiles};
							 eclipse ->
							     Results1 = [{{FName, FName}, AnnAST1} | Results],
							     Res = lists:map(fun({{FName1, NewFName1}, AST}) ->
										     {FName1, NewFName1, refac_prettypr:print_ast(AST)} end, Results1),
							     {ok, Res}
						     end;
						 false ->
						     case Editor of 
							 emacs ->
							     refac_util:write_refactored_files([{{FName, FName}, AnnAST1}]), {ok, [FName]};
							 eclipse ->
							     Res = [{FName, FName, refac_prettypr:print_ast(AnnAST1)}],
							     {ok, Res}
						     end
					     end;
					{error, Reason} -> {error, Reason}
				    end;
			       true ->
				    {error,
				     "This function is not defined in this "
				     "module; please initiate this refactoring from the  module where "
				     "it is defined."}
			    end;
			{error, Reason} -> {error, Reason}
		    end;
		{error, Reason} ->{error, Reason}
	    end;
	false -> {error, "Invalid process name."}
    end.


%% Side conditios:
%% 0. New function names, process name should not conflict with exisitng names. 
%% To add: 
%% 1. description of this refactoring.
%% 2. Rationale for this refactorign.
%% 3. This is refactoring change the program.
%% 4. Add the memoraisation functionality.
%% How this refactoring transform the program.
%% 1. The function should have an arity greater than 0. Reason: no need for message passing.
%% 2. The function should not be a recursive function, either directly or indirectly. Reason: will cause dead lock;
%% 3. The function should not be a spawned function.(because 1.spawn does not support message passing; 2: the function is already in form of process).
%% 4. The function or functions called by this function should not have receive expressions.eason: move the receive statement from one process to another, could 
%% result some messages fail to reach the destination process;
%% 5. The function or function called by this function should not register self(), but registrating the spawned processes is OK.
%% 6. The uses of self(), this depends on how self is used.
%% recieve -> send (not OK).
%% receive (not OK).
%% send ->receive (OK). (How do you decide this?)
%% send (OK).
pre_cond_check(FName, AnnAST, Pos, ModName,FunName,Arity, _ProcessName)->
    case Arity ==0 of 
	true -> {error, "This function does not have any parameters.\n"};
	false -> {ok, FunDef} = refac_util:pos_to_fun_def(AnnAST, Pos),    
		 case is_recursive_fun([FName], {ModName, FunName, Arity, FunDef}) of   %% TOdO: need to check the client modules too.
		     true ->
			 {error, "The function is a recursive (direct or indirect) function.\n"};
		     false ->
			 ok
		 end
    end.
	
		     
	    

do_fun_to_process(AnnAST, ModName, DefPos, FunName, Arity, ProcessName) ->
    RpcFunName = atom_to_list(FunName)++ "_rpc",
    {AnnAST1,_}=refac_util:stop_tdTP(fun fun_call_to_rpc/2, AnnAST, {ModName,FunName, Arity, ProcessName, RpcFunName}),
    AnnAST2 = fun_to_process(AnnAST1, FunName, DefPos, ProcessName),
    AnnAST2.

fun_call_to_rpc(Node, {ModName, FunName, Arity, ProcessName, RpcFunName}) ->
    Message = fun (Pos) -> io:format("WARNING: function ***apply*** is used at location({line, col}):~p, and wrangler " 
				     "could not decide whether this site should be refactored, please check manually!\n",
				     [Pos])
	      end,
    F1 = fun(T, _Others) ->
		case refac_syntax:type(T) of
		    arity_qualifier -> 
			Fun = refac_syntax:arity_qualifier_body(T),
			FunName1 = refac_syntax:atom_value(Fun),
			Arg = refac_syntax:arity_qualifier_argument(T),
			Arg1 = refac_syntax:integer_value(Arg),
			io:format("arg1:\n~p\n", [Arg1]),
			DefMod = get_fun_def_mod(Fun),
			io:format("DefMod:\n~p\n", [DefMod]),
			if (FunName1 == FunName) and (Arg1 == Arity) and (DefMod == ModName) ->
				 refac_syntax:copy_attrs(T,refac_syntax:arity_qualifier(refac_syntax:atom(RpcFunName), refac_syntax:integer(2)));			
			   true -> T
			end;
		    _ -> T
		end
	end,
    case refac_syntax:type(Node) of 
	attribute -> Node1=refac_util:full_buTP(F1, Node, {}),
		     {Node1, true};
	application ->
	    Op = refac_syntax:application_operator(Node),
	    Ann = refac_syntax:get_ann(Op),
	    Arguments = refac_syntax:application_arguments(Node),
	    {value, {fun_def, {Mod, F, A, _, _}}} = lists:keysearch(fun_def,1, Ann), %% any failure cases?
	    case {Mod, F, A} of 
		{ModName, FunName, Arity} ->
		    Op1 =  refac_syntax:atom(RpcFunName),
		    Arg1 = refac_syntax:atom(ProcessName),
		    Arg2 =  refac_syntax:tuple(Arguments),			   			       
		    Node1 = refac_syntax:application(Op1, [Arg1, Arg2]),
		    {Node1, true};
		{erlang, apply, 2} ->
		    Arg1 = lists:nth(1, Arguments),
		    Arg2 = lists:nth(2, Arguments),
		    case refac_syntax:type(Arg1) of 
			implicit_fun ->
			    Name = refac_syntax:implicit_fun_name(Arg1),
			    B1 = refac_syntax:atom_value(refac_syntax:arity_qualifier_body(Name)),
			    A1 = refac_syntax:integer_value(refac_syntax:arity_qualifier_argument(Name)),
			    case {B1, A1} of 
				{FunName, Arity} ->
				    F2 = refac_syntax:implicit_fun(refac_syntax:atom(RpcFunName), refac_syntax:integer(2)),
				    P = refac_syntax:atom(ProcessName),
				    T2 = case refac_syntax:type(Arg2) of 
					     list -> refac_syntax:tuple(refac_syntax:list_elements(Arg2));
					     _  -> refac_syntax:application(refac_syntax:atom(list_to_tuple), [Arg2])						   
					 end,
				    T3 = refac_syntax:list([P, T2]),
				    {refac_syntax:copy_attrs(Node, refac_syntax:application
											  (Op, [F2,T3])), true};
				
				_ -> {Node, false}
			    end;
			_ -> {Node, false}
		       end;
		  {erlang, apply, 3} ->
		      [Mod1,Fun1,Args1] = Arguments,
		      Mod2 = refac_util:try_evaluation([refac_syntax:revert(Mod1)]),  %% TODO: add backward slicing.
		      Fun2 = refac_util:try_evaluation([refac_syntax:revert(Fun1)]),
		      Pos = refac_syntax:get_pos(Node),
		      case Fun2 of 
			  {value, FunName} ->
			      case Mod2 of 
				  {value,ModName} ->
				      case refac_syntax:type(Args1) of 
					  list ->
					      case refac_syntax:list_length(Args1) of 
						  Arity ->
						      Op1 =  refac_syntax:atom(RpcFunName),
						      Arg1 = refac_syntax:atom(ProcessName),
						      Arg2 =  refac_syntax:tuple(refac_syntax:list_elements(Args1)),			   			       
						      Node1 = refac_syntax:application(Op1, [Arg1, Arg2]),
						      {Node1, true};
						  _ -> {Node, true}
					      end;
					  _ -> Message(Pos),
					       {Node, true}
				      end;
				  {value, _}-> {Node, true};
				  {error, _Reason} -> case refac_syntax:type(Args1) of 
							  list -> case refac_syntax:list_length(Args1) of 
								      Arity -> Message(Pos),
									       {Node, true};
								      _ -> {Node, true}
								  end;
							  _ -> Message(Pos),
							       {Node, true}
						      end
			      end;
			  {value, _} -> {Node, true};
			  {error, _Reason} ->  
			      case Mod2 of 
				  {value, ModName} ->
				      case refac_syntax:type(Args1) of 
					  list -> case refac_syntax:list_length(Args1) of 
						      Arity -> Message(Pos),
							       {Node, true};
						      _ -> {Node, true}
						  end;
					  _ -> Message(Pos),
					       {Node, true}
				      end;
				  {value, _} -> {Node, true};
				  {error, _Reason} -> case refac_syntax:type(Args1) of 
							  list-> case refac_syntax:list_length(Args1) of 
								     Arity -> Message(Pos),
									      {Node, true};
								     _ -> {Node, true}
								 end;
							  _  -> Message(Pos),
								{Node, true}
						      end
			      end
		      end;     
 
		_ ->
		    {Node, false}
	    end;
	_ ->{Node, false}
    end.
	


rpc_fun(FunName) ->
    RpcFunName = atom_to_list(FunName)++ "_rpc",
    RpcFun=RpcFunName++"(RegName, Request) ->
			   case whereis(RegName) of 
				 undefined -> register(RegName, spawn(fun "++atom_to_list(FunName)++"/0));
				 _ -> ok
			   end,
		           RegName ! {self(), Request},
		           receive
				 {RegName, Response} -> Response
			   end.",
    {ok, Toks, _} = refac_scan:string(RpcFun),
    {ok, Form} =erl_parse:parse_form(Toks),
    FunDef= hd(refac_syntax:form_list_elements(refac_recomment:recomment_forms([Form], []))),
    FunDef.

fun_to_process(AnnAST, FunName, DefPos, ProcessName) -> 				
     Forms = refac_syntax:form_list_elements(AnnAST),
     F = fun(Form) ->
 		case refac_syntax:type(Form) of 
 		    function -> case get_fun_def_loc(Form) of 
 				    DefPos -> 
 					[rpc_fun(FunName), fun_to_process(Form, ProcessName)];
 				    _ -> [Form]
				end;
 		    _ -> [Form] 
 		end
 	end,		
     refac_syntax:form_list([T|| Form<-Forms, T <- F(Form)]).

fun_to_process(FunDef, ProcessName)->
    Name = refac_syntax:function_name(FunDef),
    Cs = refac_syntax:function_clauses(FunDef),
    Cs1 = lists:map(fun(C) -> Ps = refac_syntax:clause_patterns(C), 
			      Guard = refac_syntax:clause_guard(C),
			      Body = refac_syntax:clause_body(C),
			      LastE = lists:last(Body),
			      Msg = refac_syntax:tuple([refac_syntax:atom(ProcessName), LastE]),
			      Dest = refac_syntax:variable('From'),
			      SendExp = refac_syntax:infix_expr(Dest, refac_syntax:operator('!'), Msg),
			      RecExp = refac_syntax:application(Name, []),
			      Body1 = lists:reverse([RecExp,SendExp | tl(lists:reverse(Body))]),
			      Ps1 = refac_syntax:tuple(Ps),				    
			      P = refac_syntax:tuple([refac_syntax:variable('From') ,Ps1]),
			      refac_syntax:clause([P], Guard, Body1)
		    end, Cs),					  
    ReceiveExp = refac_syntax:receive_expr(Cs1),
    C = refac_syntax:clause(none, [ReceiveExp]),		    
    NewFun = refac_syntax:function(Name, [C]),
    NewFun.

get_fun_def_loc(Node) ->
     As = refac_syntax:get_ann(Node),
     case lists:keysearch(fun_def, 1, As) of 
	  {value, {fun_def, {_M, _N, _A, _P, DefinePos}}} -> DefinePos;
	 _ -> false
     end.

fun_to_process_in_client_modules(Files, ModName, FunName, Arity, ProcessName, SearchPaths) ->
    case Files of 
	[] ->
	     [];
	[F | Fs] ->
	    io:format("The current file under refactoring is:\n~p\n", [F]),
	    case refac_util:parse_annotate_file(F, true, SearchPaths) of
		{ok, {AnnAST, _Info}} ->
		    {AnnAST1, Changed} = fun_to_process_in_client_modules_1(AnnAST, ModName, FunName, Arity, ProcessName),			  
		    if Changed ->
			    [{{F, F}, AnnAST1} | fun_to_process_in_client_modules(Fs, ModName,FunName, Arity, ProcessName, SearchPaths)];
		       true ->
			    fun_to_process_in_client_modules(Fs, ModName, FunName, Arity, ProcessName, SearchPaths)
		    end;
		{error, Reason} -> {error, Reason}
	    end
    end.


fun_to_process_in_client_modules_1(AnnAST, ModName, FunName, Arity, ProcessName) ->
    RpcFunName = atom_to_list(FunName)++ "_rpc",
    {AnnAST1,_}=refac_util:stop_tdTP(fun fun_call_to_rpc/2, AnnAST, {ModName, FunName, Arity, ProcessName, RpcFunName}),
    AnnAST1.

   

get_fun_def_mod(Node) ->
    As = refac_syntax:get_ann(Node),
    case lists:keysearch(fun_def, 1, As) of
      {value, {fun_def, {M, _N, _A, _P, _DefinePos}}} -> M;
      _ -> false
    end.



is_recursive_fun(Files, {ModName, FunName, Arity, FunDef}) ->
    case is_direct_recursive_fun(ModName, FunName, Arity, FunDef) of 
	true -> 
	    true;
	false ->
	    CallGraph= refac_util:build_callgraph(Files, []),
	    #callgraph{scc_order = Sccs, external_calls = _E} = refac_callgraph:construct(CallGraph),
	    Sccs1 =[[Fun||{Fun, _FunDef}<-Scc]||Scc<-Sccs],
	    lists:any(fun(E)-> (length(E)>1) andalso (lists:member({ModName, FunName, Arity}, E)) end,
		      Sccs1)
   
    end.
	

is_direct_recursive_fun(ModName, FunName, Arity, FunDef) ->
    F = fun(Node, {Mod, Fun, Ari}) ->
		case refac_syntax:type(Node) of 
		    application ->
			Op = refac_syntax:application_operator(Node),
			case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Op)) of 
			    {value, {fun_def, {Mod, Fun, Ari, _, _}}} ->
				{true, true};
			    _ -> {[],false}
			end;
		    _ -> {[], false}
		end
	end,	   
    R = refac_util:once_tdTU(F,  FunDef, {ModName, FunName, Arity}),
    case R of 
	{_, true} ->
	     true;
	_ -> false
    end.
