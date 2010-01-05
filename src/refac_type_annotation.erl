%% Copyright (c) 2010, Huiqing Li, Simon Thompson
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMIATTED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


%%---------------------------------------------------------------------------%%
%% Atoms in Erlang have multiple roles. An atom could be a module name, a function 
%% name, a process name, or just an atom literal. While in most cases, it is possible 
%% to infer the role played by an atom syntactically, in some cases it is not so obvious. 
%% This module provides functionalities that try to infer the role of an atom played using
%% some type inference techniques. We distinguish those atoms that representing modules names, 
%% function names, and process names. For an atom that represents a function name, we also 
%% try to infer the function's defining module and the function'a arity.

-module(refac_type_annotation).

-export([type_ann/2]).

-export([try_eval/2]).

%% -define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(__String, __Args), ?wrangler_io(__String, __Args)).
-else.
-define(debug(__String, __Args), ok).
-endif.


-include("../include/wrangler.hrl").

type_ann(File, SearchPaths) ->
    {ok, {AnnAST, _Info1}} = refac_util:parse_annotate_file(File, true, SearchPaths),
    start_type_env_process(),
    Fs = refac_syntax:form_list_elements(AnnAST),
    Fs1 = lists:map(fun do_type_ann/1, Fs),  %% TODO: CALLGRAPH IS NEEDED HERE!
    AnnAST1 = refac_syntax:form_list(Fs1),
    case get_all_type_info() of 
	[] ->
	    stop_type_env_process(),
	    AnnAST1;
	TypeInfo ->
	    ?debug("Typeinfo:\n~p\n", [TypeInfo]),
	    stop_type_env_process(),
	    %% propagate type info form variables to atoms 
	    %% to which the variables are bound to.
	    prop_type_info(AnnAST1, TypeInfo)
    end.


%% Annotate a function.
%%NOTE:LIMITATION HERE: How to handle functions with multiple clauses?
do_type_ann(F) ->
    case refac_syntax:type(F) of
	function->   
	    Name = refac_syntax:function_name(F),
	    Cs = [do_type_ann_clause(C,refac_syntax:clause_patterns(C)) ||
		     C <- refac_syntax:function_clauses(F)],
	    refac_syntax:function(Name, Cs);	    
	_ ->
	    F
    end.

%% Annotate a function clause.
%% LIMITATION HERE: does not handle complex patterns yet.
do_type_ann_clause(Node, Pats) ->
    Ann = refac_syntax:get_ann(Node),
    {value, {fun_def, {M, F, A, _, _}}}=lists:keysearch(fun_def,1,Ann),
    C =refac_util:full_buTP(fun do_type_ann_clause_1/2, Node, Pats),
    TypeInfo = get_all_type_info(),
    Fun= fun(P) ->
		 case refac_syntax:type(P) of
		     variable ->
			 As = refac_syntax:get_ann(P),
			 case lists:keysearch(def,1,As) of
			     {value, {def, [DefinePos]}} ->
				 case lists:keysearch(DefinePos,1, TypeInfo) of
				     {value, {DefinePos, {type, T}}} ->
					 T;
				     _ -> any
				 end;
			     _ -> any
			 end;
		     _ -> any
		 end
	 end,
    Ts = {{M, F, A}, {lists:map(Fun, Pats),any}},
    add_to_type_env([Ts]),
    ?debug("Ts:\n~p\n", [Ts]),
    C.

do_type_ann_clause_1(Node, Pats) ->
    Fun=fun(N, Type) ->
		case refac_syntax:type(N) of
		    variable ->
			?debug("Type:\n~p\n", [Type]),
			add_type_info({type, Type}, N);
		    _ ->
			N
		end
	end,
    case refac_syntax:type(Node) of
	application ->
	    Op = refac_syntax:application_operator(Node),
	    Args = refac_syntax:application_arguments(Node),
	    Arity = length(Args),
	    case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Op)) of 
		{value, {fun_def, {M, F, A, _, _}}} 
		when M=/='_' andalso F=/='_'  -> %% both module name and function name are known;
		    Args1 = do_type_ann_args({M, F, A}, map_args(Pats, Args), Args),
		    refac_util:rewrite(Node, refac_syntax:application(Op, Args1));
		_ -> %% Either module name or function name is not an atom;
		    case refac_syntax:type(Op) of
			variable ->
			    Op1 = add_type_info({type, {f_atom, ['_', try_eval(Op, fun is_atom/1), Arity]}}, Op),
			    refac_util:rewrite(Node, refac_syntax:application(Op1, Args));
			module_qualifier ->
			    M = refac_syntax:module_qualifier_argument(Op),
			    F = refac_syntax:module_qualifier_body(Op),
			    M1 = Fun(M, m_atom),
			    F1 = Fun(F, {f_atom, [try_eval(M, fun is_atom/1),try_eval(F, fun is_atom/1), Arity]}),
			    Op1 = refac_util:rewrite(Op, refac_syntax:module_qualifier(M1, F1)),
			    refac_util:rewrite(Node, refac_syntax:application(Op1, Args));
			tuple -> 
			    case refac_syntax:tuple_elements(Op) of
				[M, F] ->
				    M1 = Fun(M, m_atom),
				    F1 = Fun(F, {f_atom, [try_eval(M, fun is_atom/1), try_eval(M, fun is_atom/1), Arity]}),
				    Op1 = refac_util:rewrite(Op, refac_syntax:tuple([M1, F1])),
				    refac_util:rewrite(Node, refac_syntax:application(Op1, Args));
				_ ->
				    Node
			    end;
			_ -> Node
		    end
	    end;
	_ -> Node
    end.

do_type_ann_args({M, F, A}, MappedArgs, Args) ->
    case type(M, F, A) of 
	none -> 
	    case get_type_info({M, F, A}) of 
		none -> Args;
		{ParTypes, _RtnTypes} ->
		    ?debug("ParTypes:\n~p\n", [ParTypes]),
		    do_type_ann_args_1(ParTypes, MappedArgs, Args)
	    end;
	{ParTypes, _RtnType} ->
	    do_type_ann_args_1(ParTypes, MappedArgs, Args)	    
    end.

do_type_ann_args_1(ParTypes, MappedArgs, Args) ->
    lists:map(fun({T,A}) ->
		      ?debug("AT:\n~p\n", [{A, T}]),
		      case T of 
			  any ->
			      A;
			  _  when is_function(T)->
			      ?debug("MappedArgS:\n~p\n",[MappedArgs]),
			      ?debug("TMappedArgs:\n~p\n", [T(MappedArgs)]),
			      add_type_info({type, T(MappedArgs)}, A);
			  {f_atom, [M, F, Arity]} ->
			      ?debug("T:\n~p\n", [T]),
			      M1 = case is_function(M) of
				       true -> M(MappedArgs);
				       _ -> M
				   end,
			      F1 = case is_function(F) of
				       true ->
					   F(MappedArgs);
				       _ ->M
				   end,
			      Arity1 = case is_function(Arity) of
					  true ->
					    Arity(MappedArgs);
					  _ -> Arity
				      end,
			      ?debug("M1:\n~p\n", [M1]),
                              ?debug("F1:\n~p\n", [F1]),
			      ?debug("Arity1:\n~p\n", [Arity1]),
			      add_type_info({type, {f_atom, [M1, F1, Arity1]}}, A);
			  _ ->
			      ?debug("T:\n~p\n", [T]),
			      ?debug("A:\n~p\n", [A]),
			      add_type_info({type, T}, A)
		      end
	      end, lists:zip(ParTypes, Args)).


type(erlang, apply, 3) ->
    {[m_atom, fun(Args)->
		      case Args of 
			  [A1, A2, A3] ->
			      {f_atom, [try_eval(A1, fun is_atom/1), try_eval(A2, fun is_atom/1),
					try_eval_length(A3)]};
			  _ -> {f_atom, ['_','_','_']}
		      end
	      end, any], any};
type(_, _, _) ->
    none.


try_eval(Expr,Cond) when is_function(Expr) ->
    fun(X)->try_eval(Expr(X), Cond) end;
try_eval(Expr, Cond) ->
    
    E1 = refac_syntax:revert(Expr),
    ?debug("E1:\n~p\n", [E1]),
    try erl_eval:expr(E1, []) of 
	{value, V, _} ->
	    ?debug("V:\n~p\n", [V]),
	    case Cond(V) of 
		true -> V;
		_ -> '_'
	    end
    catch 
	_E1:_E2 -> 
	    ?debug("Expr:\n~p\n", [Expr]),
	    As = refac_syntax:get_ann(Expr),
	    case lists:keysearch(value, 1, As) of 
		{value, {value, {{_,V}, _DefPos}}} ->
		    case Cond(V) of 
			true ->
			    ?debug("V:\n~p\n",[V]),   
			    V;
			_ -> '_'
		    end;
		_ -> '_'
	    end	    
    end.

try_eval_length(Expr) when is_function(Expr) ->
    fun(X)->try_eval_length(Expr(X)) end; 
try_eval_length(Expr) ->
    E = refac_syntax:set_pos(refac_syntax:application(
				refac_syntax:set_pos(
				  refac_syntax:atom(length),{1,1}), [Expr]), {1,1}),
    E1 = refac_syntax:revert(E),
    try erl_eval:expr(E1, []) of 
	{value, V, _} ->
	    V
    catch 
	_E1:_E2 -> 
	    As = refac_syntax:get_ann(Expr),
	    ?debug("As:\n~p\n", [As]),
	    case lists:keysearch(value, 1, As) of 
		{value, {value, {{list, V}, _DefPos}}} ->
		  V;
		_ -> 
		    ?debug("Expr:\n~p\n", [Expr]),
		    case refac_syntax:type(Expr) of 
			list -> refac_syntax:list_length(Expr);
			_ -> '_'
		    end
	    end	    
    end.
    

add_type_info({type, Type}, Node) ->
    As =refac_syntax:get_ann(Node),
    Ps = [{Pos, {type, Type}}|| {value, {_,  Pos}} <- As],
    Ps1 =lists:append([[{Pos, {type, Type}}|| Pos<-Poss] ||{def, Poss} <-As]),
    ?debug("Ps:\n~p\n", [Ps]),
    ?debug("Ps1:\n~p\n", [Ps1]),
    add_to_type_env(Ps++Ps1),
    Node1 = refac_syntax:add_ann({type, Type}, Node),
    ?debug("Node1:\n~p\n", [Node1]),
    Node1.

start_type_env_process() ->
    Pid = spawn_link(fun() -> type_env_loop([]) end), 
    register(type_env, Pid).

stop_type_env_process() ->
    type_env ! stop.

%% init_type_env_process() ->
%%      type_env ! init.

get_all_type_info() ->
    type_env ! {self(), get_all},
    receive 
	{type_env, Res} ->
	    Res
    end.

get_type_info(Key) ->    
    type_env ! {self(), get, Key},
    receive
	{type_env, Res} ->
	    Res
    end.
    

add_to_type_env([]) ->
    [];
add_to_type_env(PosTypes) ->
    type_env ! {add, PosTypes}.

	
type_env_loop(Env) ->
    receive
	{add, PosTypes} ->
	    Env1 = PosTypes++Env,
	    type_env_loop(Env1);
	{From, get, Key} ->
	    case lists:keysearch(Key,1, Env) of 
		{value, {Key, Type}} -> 
		    From ! {type_env, Type};
		false ->
		    From ! {type_env, none}
	    end,
	    type_env_loop(Env);
	{From, get_all} ->
	    From ! {type_env, Env},
	    type_env_loop(Env);
	init ->
	    type_env_loop([]);
	stop ->
	   %% ?debug("process stoped.\n"),
	    ok
    end.

prop_type_info(AnnAST, TypeEnv) ->  
    refac_util:stop_tdTP(fun do_prop_type_info/2, AnnAST, TypeEnv).
  
do_prop_type_info(Node, TypeEnv) ->
    case refac_syntax:type(Node) of 
	atom ->
	    Pos = refac_syntax:get_pos(Node),
	    Ts =lists:usort([Type||{P, Type} <-TypeEnv, P==Pos]),
	    case Ts of
		[] ->
		    {Node, true};
		_ ->
		    Node1 = lists:foldl(fun(T, N)->
						refac_syntax:add_ann(T, N)
					end, Node, Ts),
		    {Node1, true}
	    end;	    
	_ -> {Node, false}
    end.
  
map_args(Pats, ActualArgs)->
    Fun= fun(Expr) ->
		 case refac_syntax:type(Expr) of 
		     literal -> 
			 Expr;
		     variable ->
			 As = refac_syntax:get_ann(Expr),
			 case lists:keysearch(def, 1, As) of
			     {value, {def, DefinePos}} ->
				 {Ps1, _Ps2} = lists:splitwith(
						 fun(P) -> case refac_syntax:type(P) of
							       variable ->
								   As1 =refac_syntax:get_ann(P),
								   case lists:keysearch(def,1,As1) of
								       {value, {def, DefinePos}} ->
									   false;
								       _ -> true
								   end;
							       _ -> true
							   end
						 end, Pats),
				 case length(Ps1) == length(Pats) of 
				     true ->
					 Expr;
				     _ -> 
					 ?debug("nth:\n~p\n", [length(Ps1)+1]),
					 fun(P) -> lists:nth(length(Ps1)+1, P) end
				 end;
			     _ ->
				 Expr
			 end;
		     _ ->  Expr
		 end
	 end,
    Res = lists:map(Fun, ActualArgs),
    ?debug("Res:\n~p\n", [Res]),
    Res.
	

%% sort_funs(File) ->
%%      CallGraph = wrangler_callgraph_server:get_callgraph([File]),
%%      CallGraph.
     %% TrimmedSccs = trim_scc(CallGraph#callgraph.scc_order, CallGraph#callgraph.callercallee, [], []),
     %% lists:append(TrimmedSccs).

%% trim_scc([], _CallerCallee, _PFunAcc, Acc) -> lists:reverse(Acc);
%% trim_scc([Scc | Sccs], CallerCallee, PFunAcc, Acc) ->
%%     SccFuns = lists:map(fun ({Fun, _FunDef}) -> Fun end, Scc),
%%     IsProcessScc = lists:any(fun ({_Fun, FunDef}) -> is_process_related_fun(FunDef) end, Scc),
%%     CalledFuns = lists:usort(lists:flatmap(fun (Fun) ->
%% 						   case lists:keysearch(Fun, 1, CallerCallee) of
%% 						     {value, {Fun, Called}} -> Called;
%% 						     _ -> []
%% 						   end
%% 					   end,
%% 					   SccFuns)),
%%     PFunsCalled = length(lists:subtract(CalledFuns, PFunAcc)) < length(CalledFuns),
%%     case IsProcessScc orelse PFunsCalled of
%%       true -> trim_scc(Sccs, CallerCallee, SccFuns ++ PFunAcc, [Scc | Acc]);
%%       _ -> trim_scc(Sccs, CallerCallee, PFunAcc, Acc)
%%     end.

%% type(erlang, apply, 3) ->
%%     {[m_atom, f_atom, any], any};
%% type(erlang, unregister, 1) ->
%%     {[p_atom], any};
%% type(erlang, whereis, 1) ->
%%     {[p_atom], any};
%% type(_M,_F, _A)->
%%     none.
