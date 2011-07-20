%% This module shows how to use Wrangler's refactoring 
%% level API to compose more complex refactorings.
%% This example refactoring tries to rename all the 
%% function names that are in camlCase format to camel_case
%% format. An error message is issued each time the 
%% refactoring fails to rename a function because of 
%% name conflict.
%%
%% This refactoring is supposed to run in command line.
%% Before run this refactoring, you can use
%% wrangler_api:start() to start the Wrangler application.
%% Doing this will allow you to undo the refactoring 
%% if you are not happy with the result; and when you 
%% finish, you can use wrangler_api:stop() to stop the 
%% wrangler application.

%%@author  Huiqing Li <H.Li@kent.ac.uk>

%%@hidden
%%@private
-module(refac_batch_rename_fun).

-export([rename_fun_to_camel_case/1]).

rename_fun_to_camel_case(SearchPaths)->
    %% start the Wrangler application.
    Files = refac_misc:expand_files(SearchPaths, ".erl"),
    Res=[rename_in_file(File, SearchPaths)||File<-Files],
    {ok, lists:usort(lists:append(Res))}.

rename_in_file(File, SearchPaths) ->
    %% get all the functions defined in this file.
    FAs = api_refac:defined_funs(File),
    [rename_one_function(File, SearchPaths, F, A) 
     ||{F,A} <- FAs, camelCase_to_camel_case(F) /= F].
    

%% rename one function whose name is in camelCase.
rename_one_function(File, SearchPaths, F, A) ->
    NewName = camelCase_to_camel_case(F),
    refac_io:format("\nRenaming function ~p/~p to ~p/~p in ~p ...\n",
                    [F, A, NewName, A, File]),
    Res=api_wrangler:rename_fun(File, F, A, NewName, SearchPaths),
    case Res of
        {ok, FilesChanged} ->
            FilesChanged;
        {error, Reason} ->
            refac_io:format("\nRenaming ~p/~p in file, ~p, "
                            "failed: ~p.\n", 
                            [F,A,File,Reason]),
            []
    end.

%%===================================================
%% Some utility functions.

%% transform camelCase atom to camel_case.
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
    
                                      
 
