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

-compile(export_all).

%%-define(DEBUG, true).

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
    Funs = sorted_funs(File),
    Funs1 = lists:map(fun({{M,F, A},FunDef}) ->{{M, F, A},do_type_ann(FunDef)} end, Funs), 
    Fs1 = lists:map(fun(Form) ->
			    case refac_syntax:type(Form) of
				function ->
				    Ann = refac_syntax:get_ann(Form),
				    {value, {fun_def, {M, F, A, _, _}}}=lists:keysearch(fun_def,1,Ann),
				    {value, {{M, F, A}, FunDef}} =lists:keysearch({M, F, A}, 1, Funs1),
				    FunDef;
				_ -> Form
			    end
		    end, Fs),
    AnnAST1 = refac_syntax:form_list(Fs1),
    case get_all_type_info() of 
	[] ->
	    stop_type_env_process(),
	    AnnAST1;
	TypeInfo ->
	    ?debug("Typeinfo:\n~p\n", [TypeInfo]),
	    stop_type_env_process(),
	    prop_type_info(AnnAST1, TypeInfo)
    end.


do_type_ann(Form) ->
    case refac_syntax:type(Form) of
      function ->
	  Ann = refac_syntax:get_ann(Form),
	  {value, {fun_def, {M, F, A, _, _}}}=lists:keysearch(fun_def,1,Ann),
	  Name = refac_syntax:function_name(Form),
	  Cs = [refac_util:full_buTP(fun do_type_ann_clause/2, C, refac_syntax:clause_patterns(C))
		|| C <- refac_syntax:function_clauses(Form)],
	  CsPats = [refac_syntax:clause_patterns(C) || C <- refac_syntax:function_clauses(Form)],
	  TypeInfo = get_all_type_info(),
	  CsPatsTypes = [[get_pat_type(P, TypeInfo)|| P <- CPats] || CPats <- CsPats],
	  ZippedCsPatsTypes = zip_list(CsPatsTypes),
	  PatTypes = [lists:usort(Ts) || Ts <- ZippedCsPatsTypes],
	  ?debug("MFA:\n~p\n", [{M,F,A}]),
	  ?debug("ParTypes:\n~p\n", [[hd(PT)||PT<-PatTypes]]),
	  case lists:all(fun (T) -> length(T) == 1 end, PatTypes) andalso 
	       lists:any(fun(T) -> T/=[any] end, PatTypes) of
	    true ->
		Ts = {{M, F, A}, {[hd(PT)||PT<-PatTypes], any}},
		refac_io:format("Ts:\n~p\n", [Ts]),
		add_to_type_env([Ts]);
	      _ -> ok
	  end,
	  refac_syntax:function(Name, Cs);
      _ ->
	  Form
    end.
 
do_type_ann_clause(Node, Pats) ->
    case refac_syntax:type(Node) of
      application ->
	    Op = refac_syntax:application_operator(Node),
	    Args = refac_syntax:application_arguments(Node),
	    case lists:keysearch(fun_def, 1, refac_syntax:get_ann(Op)) of
	    {value, {fun_def, {M, F, A, _, _}}}
		  when
		      M =/= '_' andalso F =/= '_' -> 
		    Args1 = do_type_ann_args({M, F, A}, map_args(Pats, Args), Args),
		    refac_util:rewrite(Node, refac_syntax:application(Op, Args1));
		_ -> %% Either module name or function name is not an atom;
		    do_type_ann_op(Node)
	    end;
	_ -> Node
    end.

do_type_ann_op(Node) ->
    Fun = fun (N, Type) -> 
		  case refac_syntax:type(N) of
		      variable ->
			  ?debug("Type:\n~p\n", [Type]),
			  add_type_info({type, Type}, N);
		      _ -> N
		  end
	  end,
    Op = refac_syntax:application_operator(Node),
    Args = refac_syntax:application_arguments(Node),
    Arity = length(Args),
    case refac_syntax:type(Op) of
	variable ->
	    Op1 = add_type_info({type, {f_atom, ['_', try_eval(Op, fun is_atom/1), Arity]}}, Op),
	    refac_util:rewrite(Node, refac_syntax:application(Op1, Args));
	module_qualifier ->
	    M = refac_syntax:module_qualifier_argument(Op),
	    F = refac_syntax:module_qualifier_body(Op),
	    M1 = Fun(M, m_atom),
	    F1 = Fun(F, {f_atom, [try_eval(M, fun is_atom/1), try_eval(F, fun is_atom/1), Arity]}),
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
    end.

do_type_ann_args({M, F, A}, MappedArgs, Args) ->
    case type(M, F, A) of 
	none -> 
	    case get_type_info({M, F, A}) of 
		none -> Args;
		{ParTypes, _RtnTypes} ->
		    ?debug("do_type_ann_args:\n~p\n", [ParTypes]),
		    do_type_ann_args_1(ParTypes, MappedArgs, Args)
	    end;
	{ParTypes, _RtnType} ->
	    do_type_ann_args_1(ParTypes, MappedArgs, Args)	    
    end.

do_type_ann_args_1(ParTypes, MappedArgs, Args) ->
    ZippedParTypeArgs = lists:zip(ParTypes, Args),
    lists:map(fun ({ParType, Arg}) ->
		      ?debug("AT:\n~p\n", [{Arg, ParType}]),
		      case ParType of
			any ->
			    Arg;
			_ when is_function(ParType) ->
			    ?debug("MappedArgS:\n~p\n", [MappedArgs]),
			    ?debug("TMappedArgs:\n~p\n", [ParType(MappedArgs)]),
			    add_type_info({type, ParType(MappedArgs)}, Arg);
			{f_atom, [M, F, Arity]} ->
			    ?debug("T:\n~p\n", [ParType]),
			    M1 = case is_function(M) of
				   true -> M(MappedArgs);
				   _ -> M
				 end,
			    F1 = case is_function(F) of
				   true ->
				       F(MappedArgs);
				   _ -> M
				 end,
			    Arity1 = case is_function(Arity) of
				       true ->
					   Arity(MappedArgs);
				       _ -> Arity
				     end,
			    ?debug("M1:\n~p\n", [M1]),
			    ?debug("F1:\n~p\n", [F1]),
			    ?debug("Arity1:\n~p\n", [Arity1]),
			    add_type_info({type, {f_atom, [M1, F1, Arity1]}}, Arg);
			_ ->
			    ?debug("T:\n~p\n", [ParType]),
			    ?debug("A:\n~p\n", [Arg]),
			    add_type_info({type, ParType}, Arg)
		      end
	      end, ZippedParTypeArgs).


add_type_info({type, Type}, Node) ->
    As =refac_syntax:get_ann(Node),
    Ps = [{Pos, {type, Type}}|| {value, {_,  Pos}} <- As],
    Ps1 =lists:append([[{Pos, {type, Type}}|| Pos<-Poss] ||{def, Poss} <-As]),
    add_to_type_env(Ps++Ps1),
    refac_syntax:add_ann({type, Type}, Node).
   

map_args(Pats, ActualArgs) ->
    Fun = fun (ActualArg) ->
		  case refac_syntax:type(ActualArg) of
		      literal ->
			  ActualArg;
		      variable ->
			  As = refac_syntax:get_ann(ActualArg),
			  case lists:keysearch(def, 1, As) of
			      {value, {def, DefinePos}} ->
				  {Ps1, _Ps2} = lists:splitwith(
					      fun (P) -> case refac_syntax:type(P) of
							     variable ->
								 As1 = refac_syntax:get_ann(P),
								 case lists:keysearch(def, 1, As1) of
								     {value, {def, DefinePos}} ->
									 false;
								     _ -> true
								 end;
							     _ -> true
							 end
					      end, Pats),
				  case length(Ps1) == length(Pats) of
				      true ->
					  ActualArg;
				      _ ->
					  ?debug("nth:\n~p\n", [length(Ps1) + 1]),
					  fun (P) -> lists:nth(length(Ps1) + 1, P) end
				  end;
			      _ ->
				  ActualArg
			  end;
		      _ -> ActualArg
		  end
	  end,
    Res = lists:map(Fun, ActualArgs),
    ?debug("Res:\n~p\n", [Res]),
    Res.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

get_pat_type(P, TypeInfo) ->
    case refac_syntax:type(P) of
	variable ->
	    As = refac_syntax:get_ann(P),
	    case lists:keysearch(def, 1, As) of
		{value, {def, [DefinePos]}} ->
		    case lists:keysearch(DefinePos, 1, TypeInfo) of
			{value, {DefinePos, {type, T}}} ->
			    T;
			_ -> any
		    end;
		_ -> any
	    end;
	_ -> any
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%%                 Propagate Type Information to Atoms                   %%
%%                                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
		    Type = element(2, lists:unzip(Ts)),
		    Node1 = refac_syntax:add_ann({type, Type}, Node),
		    refac_io:format("AtomNodeWithTypeInfo:\n~p\n", [Node1]),
		    {Node1, true}
	    end;	    
	_ -> {Node, false}
    end.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%%                 Type Info Server                                      %%
%%                                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_type_env_process() ->
    Pid = spawn_link(fun() -> type_env_loop([]) end), 
    register(type_env, Pid).

stop_type_env_process() ->
    type_env ! stop.


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
	stop ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%%                 Utility Functions                                     %%
%%                                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try_eval(Expr, Cond) when is_function(Expr) ->
    fun (X) -> try_eval(Expr(X), Cond) end;
try_eval(Expr, Cond) ->
    E = refac_syntax:revert(Expr),
    try
      erl_eval:expr(E, [])
    of
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
		{value, {value, {{_, V}, _DefPos}}} ->
		    case Cond(V) of
			true ->
			    ?debug("V:\n~p\n", [V]),
			    V;
			_ -> '_'
		    end;
		_ -> '_'
	    end
    end.

try_eval_length(Expr) when is_function(Expr) ->
    fun (X) -> try_eval_length(Expr(X)) end;
try_eval_length(Expr) ->
    E = refac_syntax:revert(mk_length_app(Expr)),
    try
      erl_eval:expr(E, [])
    of
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

mk_length_app(Expr) ->
    refac_syntax:set_pos(refac_syntax:application(
			   refac_syntax:set_pos(
			     refac_syntax:atom(length), {1, 1}), [Expr]), {1, 1}).


zip_list(ListOfLists) ->    
    zip_list_1(ListOfLists, []).
		      
zip_list_1([[]|_T], Acc)  ->
    Acc;
zip_list_1(ListOfLists, Acc)->      
    zip_list_1([tl(L) || L <-ListOfLists], Acc ++ [[hd(L)|| L  <- ListOfLists]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%%                 Hard Coded Type Info                                  %%
%%                                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
type(erlang, apply, 3) ->
    {[m_atom, fun(Args)->
		      [A1, A2, A3] = Args, 
		      {f_atom, [try_eval(A1, fun is_atom/1), try_eval(A2, fun is_atom/1),
				try_eval_length(A3)]}
	      end, any], any};
type(_, _, _) -> 
    none.

    
sorted_funs(File) ->
   CallGraph = wrangler_callgraph_server:get_callgraph([File]),
   lists:append(CallGraph#callgraph.scc_order).

