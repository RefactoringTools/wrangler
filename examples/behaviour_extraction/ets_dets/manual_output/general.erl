-module(general).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").

-compile(export_all).

-ifndef(BUG_OPEN_FILE).
-define(BUG_OPEN_FILE,false).
-endif.

-ifndef(BUG_INSERT_NEW).
-define(BUG_INSERT_NEW,true).
-endif.

-define(BEH_INSTANCE, dets_eqc).

-record(state,{name,type,contents=[]}).

behaviour_info(callbacks) ->
    [{opened, 1},{init_state, 1},{get_contents, 1},
     {parallel_set_up, 0},{set_up, 0},{insert_postcondition, 0},
     {delete_postcondition, 0},{particular_refactoring, 1},
     {particular_next_state_data, 5}, {module_name, 0},{table_name, 0}];
behaviour_info(_Other) -> undefined.

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

init_state(S) -> ?BEH_INSTANCE:init_state(S).

opened(S) -> ?BEH_INSTANCE:opened(S).

next_state_data(From, To, S, V, Call) ->
    ?BEH_INSTANCE:particular_next_state_data(From, To, S, V, Call).

%% Precondition (for state data).
%% Precondition is checked before command is added to the command sequence
precondition(From,_To,S,{call,_,open_file,[_,[{type,T}]]}) ->
    lists:member(S#state.type,[undefined,T])
      andalso (?BEH_INSTANCE:particular_refactoring(From));
precondition(_From,_To,_S,{call,_,insert_new,_}) ->
    ?BUG_INSERT_NEW;
precondition(_From,_To,_S,{call,_,_,_}) ->
    true.

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state_data(From,To,S,_,<command>) 
postcondition(_From,_To,_S,{call,_,close,_},Res) ->
    Res==ok orelse Res=={error,not_owner};  %% not_owner in parallel tests
postcondition(_From,_To,_S,{call,_,delete,_},Res) ->
    Res == ?BEH_INSTANCE:delete_postcondition();
postcondition(_From,_To,_S,{call,_,insert,_},Res) ->
    Res == ?BEH_INSTANCE:insert_postcondition();
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

prop_general() ->
    ?FORALL(Cmds, (more_commands(3,commands(?MODULE))),
	    ?TRAPEXIT(
	       begin
		   ?BEH_INSTANCE:set_up(),
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
		?BEH_INSTANCE:parallel_set_up(),
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
			       
file_delete(Name) ->
    case file:delete(Name) of
	{error,enoent} ->
	    ok;
	_Bad ->
	    file_delete(Name)
    end.

get_contents(Table) -> ?BEH_INSTANCE:get_contents(Table).
