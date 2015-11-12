-module(dets_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-compile(export_all).

-ifndef(BUG_OPEN_FILE).
-define(BUG_OPEN_FILE,false).
-endif.

-ifndef(BUG_INSERT_NEW).
-define(BUG_INSERT_NEW,true).
-endif.

-record(state,{name,type,contents=[]}).

module_name() -> dets.

table_name() -> dets_table.

%% Definition of the states. Each state is represented by a function, 
%% listing the transitions from that state, together with generators 
%% for the calls to make each transition.
init_state(_S) ->
    [{opened,{call,?MODULE,open_file,[table_name(),[{type,oneof([set,bag])}]]}}
     ].

opened(S) ->
    [{init_state,{call,module_name(),close,[table_name()]}},
     {opened,{call,?MODULE,open_file,[table_name(),[{type,S#state.type}]]}},
   {opened,{call,module_name(),insert,[table_name(),oneof([object(),list(object())])]}},
   {opened,{call,module_name(),insert_new,[table_name(),oneof([object(),list(object())])]}},
   {opened,{call,module_name(),delete,[table_name(),nat()]}},
   {opened,{call,?MODULE,get_contents,[table_name()]}}
     ].

object() ->
    {nat(),nat()}.

pattern() ->
    {oneof(['$1',nat()]),oneof(['$1','$2',nat()])}.

%% Identify the initial state
initial_state() ->
    init_state.

%% Initialize the state data
initial_state_data() ->
    #state{}.

%% Next state transformation for state data.
%% S is the current state, From and To are state names
next_state_data(_From,_To,S,V,{call,_,open_file,[_,[{type,T}]]}) ->
    S#state{name=V,type=T};
next_state_data(_From,_To,S,_V,{call,_,insert,[_,Objs]}) ->
    S#state{contents=model_insert(S#state.type,S#state.contents,Objs)};
next_state_data(_From,_To,S,_V,{call,_,insert_new,[_,Objs]}) ->
    case any_exist(Objs,S#state.contents) of
	true ->
	    S;
	false ->
	    S#state{contents=model_insert(S#state.type,S#state.contents,Objs)}
    end;
next_state_data(_From,_To,S,_V,{call,_,delete,[_,K]}) ->
    S#state{contents=model_delete(S#state.contents,K)};
next_state_data(_From,_To,S,_V,{call,_,_,_}) ->
    S.

%% Precondition (for state data).
%% Precondition is checked before command is added to the command sequence
precondition(From,_To,S,{call,_,open_file,[_,[{type,T}]]}) ->
    lists:member(S#state.type,[undefined,T])
	andalso (From==init_state orelse ?BUG_OPEN_FILE);
precondition(_From,_To,_S,{call,_,insert_new,_}) ->
    ?BUG_INSERT_NEW;
precondition(_From,_To,_S,{call,_,_,_}) ->
    true.

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state_data(From,To,S,_,<command>) 
postcondition(_From,_To,_S,{call,_,close,_},Res) ->
    Res==ok orelse Res=={error,not_owner};  %% not_owner in parallel tests
postcondition(_From,_To,_S,{call,_,delete,_},Res) ->
    Res==ok;
postcondition(_From,_To,_S,{call,_,insert,_},Res) ->
    Res==ok;
postcondition(_From,_To,S,{call,_,insert_new,[_,Objs]},Res) ->
    Res==not any_exist(Objs,S#state.contents);
postcondition(_From,_To,S,{call,_,get_contents,_},Res) ->
    lists:sort(Res) == lists:sort(S#state.contents);
postcondition(_From,_To,_S,{call,_,_,_},_Res) ->
    true.

%% Model

model_insert(set,S,{K,_V}=Obj) ->
    lists:keydelete(K,1,S)++[Obj];
model_insert(bag,S,{_K,_V}=Obj) ->
    (S--[Obj])++[Obj];   % surprise! why can't Obj appear twice?
model_insert(T,S,[Obj|Objs]) ->
    model_insert(T,model_insert(T,S,Obj),Objs);
model_insert(_,S,[]) ->
    S.

any_exist(Obj,S) when is_tuple(Obj) ->
    any_exist([Obj],S);
any_exist(Objs,S) ->
    [K || {K,_} <- Objs,
	  lists:keymember(K,1,S)]
	/= [].

model_delete(S,K) ->
    [O || O={K1,_} <- S,
	  K1/=K].

%% Top level property

prop_dets() ->
    ?FORALL(Cmds, (more_commands(3,commands(?MODULE))),
	    ?TRAPEXIT(
	       begin
		   dets:close(table_name()),
		   file_delete(table_name()),
		   {H,S,Res} = run_commands(?MODULE,Cmds),
		   ?WHENFAIL(
		      (io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,Res])),
		      (aggregate(command_names(Cmds),
				 Res == ok)))
	       end)).

%% Parallel testing property

prop_parallel() ->
    fails(
    %% Number of attempts to make each test case fail. When searching
    %% for a failing example, we run each test once. When searching for
    %% a way to shrink a test case, we run each candidate shrinking
    %% 100 times.
    ?FORALL(Attempts,?SHRINK(1,[100]),
      ?FORALL({Seq,Par}, (parallel_commands(?MODULE)),
	?ALWAYS(Attempts,
	  ?TIMEOUT(1000,
	    begin	
		%% Make sure the table is not left open by a previous test.
		[dets:close(table_name()) || _ <- "abcdefghijkl"],
		%% Make sure the table is initially empty.
		file_delete(table_name()),
		%% Run the test case.
		{H,ParH,Res} = 
		    run_parallel_commands(?MODULE,{Seq,Par}),
		?WHENFAIL(
		   (io:format("History: ~p\nParallel: ~p\n\nRes: ~p\n",
			      [H,ParH,Res])),
		   (collect(length(Par),
			    aggregate([length(P) || P <- Par],
				      collect(length([ok || P <- Par,
							    {set,_,{call,_,open_file,_}} <- P]),
				      Res == ok)))))
	    end))))
     ).


%% Weight for transition (this callback is optional).
%% Specify how often each transition should be chosen
weight(_From,_To,{call,_,_,_}) ->
    1.

open_file(Name,Args) ->
    {ok,N} = dets:open_file(Name,Args),
    N.

%% Operations for inclusion in test cases

get_contents(Name) ->
    dets:traverse(Name,fun(X)->{continue,X}end).
			       
file_delete(Name) ->
    case file:delete(Name) of
	{error,enoent} ->
	    ok;
	_Bad ->
	    file_delete(Name)
    end.

