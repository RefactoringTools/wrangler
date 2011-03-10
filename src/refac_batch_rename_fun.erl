-module(refac_batch_rename_fun).

-export([rename_fun_to_camel_case/1]).

rename_fun_to_camel_case(SearchPaths)->
    wrangler_api:start(),
    Files = refac_api:expand_files(SearchPaths, ".erl"),
    Res=[rename_in_file(File, SearchPaths)||File<-Files],
    wrangler_api:stop(),
    {ok, lists:usort(lists:append(Res))}.

rename_in_file(File, SearchPaths) ->   
    FAs = refac_api:defined_funs(File),
    Res=[rename_one_function(File, SearchPaths, F, A) 
         ||{F,A} <- FAs, camelCase_to_camel_case(F) /= F],
    lists:append(Res).

rename_one_function(File, SearchPaths, F, A) ->
    NewName = camelCase_to_camel_case(F),
    refac_io:format("\nRenaming function ~p/~p to ~p/~p in ~p ...\n",
                    [F, A, NewName, A, File]),
    Res=wrangler_api:rename_fun(File, F, A, NewName, SearchPaths),
    case Res of
        {ok, FilesChanged} ->
            FilesChanged;
        {error, Reason} ->
            refac_io:format("\nRenaming ~p/~p in file, ~p, failed: ~p.\n", 
                            [F,A,File,Reason]),
            []
    end.


%% utility functions.
camelCase_to_camel_case(Name) ->
    list_to_atom(camelCase_to_camel_case_1(
                   atom_to_list(Name),[])).

camelCase_to_camel_case_1([], Acc) ->
    lists:reverse(Acc);
camelCase_to_camel_case_1([H|T], Acc) 
  when  (H >= 65) and (90 >= H)->
    case Acc of 
        [95|_] ->
            camelCase_to_camel_case_1(T, [H+(97-65)|Acc]);
        _ ->
            camelCase_to_camel_case_1(T, [H+(97-65), 95|Acc])
    end;
camelCase_to_camel_case_1([H|T], Acc) ->
      camelCase_to_camel_case_1(T, [H|Acc]).
    
                                      
 
