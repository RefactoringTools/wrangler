-module(refac_code_search_utils).


-export([var_binding_structure/1,identifier_name/1, compose_search_result_info/1,
	 display_search_results/3,start_counter_process/0,start_counter_process/1,
	 stop_counter_process/1,add_new_export_var/2,get_new_export_vars/1,
	 gen_new_var_name/1]).

-include("../include/wrangler.hrl").

%% Get the binding structure of variables.
-spec(var_binding_structure/1::([syntaxTree()]) -> [{integer(), integer()}]).      
var_binding_structure(ASTList) ->
    VarLocs = lists:keysort(2, refac_misc:collect_var_source_def_pos_info(ASTList)),
    case VarLocs of
      [] ->
	  [];
      _ -> var_binding_structure_1(VarLocs)
    end.

var_binding_structure_1(VarLocs) ->
    SrcLocs = [SrcLoc || {_Name, SrcLoc, _DefLoc} <- VarLocs],
    IndexedLocs = lists:zip(SrcLocs, lists:seq(1, length(SrcLocs))),
    Fun = fun ({_Name, SrcLoc, DefLoc}) ->
		  DefLoc1 = hd(DefLoc),
		  {value, {SrcLoc, Index1}} = lists:keysearch(SrcLoc, 1, IndexedLocs),
		  Index2 = case lists:keysearch(DefLoc1, 1, IndexedLocs) of
			       {value, {_, Ind}} -> Ind;
			       _ -> 0 %% free variable
			   end,
		  {Index1, Index2}
	  end,
    BS = [Fun(VL) || VL <- VarLocs],
    lists:keysort(1, lists:usort(BS)).
  

identifier_name(Exp) ->
    case refac_syntax:type(Exp) of
	atom ->
	    refac_syntax:atom_value(Exp);
	variable ->
	    refac_syntax:variable_name(Exp);
	_ ->throw({error, "Not an identifier"})
    end.

compose_search_result_info(Ranges) ->
    compose_search_result_info(Ranges, "").
compose_search_result_info([], Str) ->
    Str;
compose_search_result_info([{FileName, {{StartLine, StartCol}, {EndLine, EndCol}}}|Ranges], Str) ->
    Str1 =Str ++ "\n"++FileName++io_lib:format(":~p.~p-~p.~p: ", [StartLine, StartCol, EndLine, EndCol]),
    compose_search_result_info(Ranges, Str1).


 
display_search_results(Ranges, AntiUnifier, Type) ->
    case Ranges of
	[_] -> 
	    ?wrangler_io("No "++Type++" expression has been found.\n", []);
	_ -> 
	    ?wrangler_io("~p expressions (including the expression selected)"
			 " which are "++Type++" to the expression selected have been found. \n", [length(Ranges)]),
	    ?wrangler_io(compose_search_result_info(Ranges), []),
	    case AntiUnifier of 
		none -> ok;
		_ ->
		    ?wrangler_io("\nThe generalised expression would be:\n\n~s\n\n", [refac_prettypr:format(AntiUnifier)])
	    end,
	    ?wrangler_io("\n\nNOTE: Use 'M-x compilation-minor-mode' to make the result "
			 "mouse clickable if this mode is not already enabled.\n",[]),
	    ?wrangler_io("      Use 'C-c C-e' to remove highlights!\n", []),
	    {ok, Ranges}
    end.
    
start_counter_process() ->
    start_counter_process(sets:new()).

start_counter_process(UsedNames) ->
    spawn_link(fun () -> counter_loop({1, UsedNames, []}) end).

stop_counter_process(Pid) ->
    Pid!stop.

counter_loop({SuffixNum, UsedNames, NewExportVars}) ->
    receive
      {From, next} ->
	  {NewSuffixNum, NewName} = make_new_name(SuffixNum, UsedNames),
	  From ! {self(), NewName},
	  counter_loop({NewSuffixNum, sets:add_element(NewName, UsedNames), NewExportVars});
      {add, Name} ->
	  counter_loop({SuffixNum, UsedNames, [Name| NewExportVars]});
      {From, get} ->
	  From ! {self(), lists:reverse(NewExportVars)},
	  counter_loop({SuffixNum, UsedNames, NewExportVars});
      stop ->
	  ok
    end.

add_new_export_var(Pid, VarName) ->
    Pid ! {add, VarName}.

get_new_export_vars(Pid) ->
    Pid !{self(), get},
    receive
	{Pid, Vars} ->
	     Vars
    end.
	
gen_new_var_name(Pid) -> 
    Pid ! {self(), next},
    receive
	{Pid, V} ->
	     V
    end.

make_new_name(SuffixNum, UsedNames) ->
    NewName = "NewVar_"++integer_to_list(SuffixNum),
    case sets:is_element(NewName, UsedNames) of 
	true ->
	    make_new_name(SuffixNum+1, UsedNames);
	_ -> {SuffixNum, NewName}
    end.
