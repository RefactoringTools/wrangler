%%% This is an -*- Erlang -*- file.
%%%-------------------------------------------------------------------
%%% File    : gen_refac.hrl
%%%-------------------------------------------------------------------
-include("../include/wrangler.hrl").

-compile({parse_transform, expand_rule}).

-record(args, {current_file_name :: filename(), 
               cursor_pos        :: pos(), 
               highlight_range   :: {pos(), pos()}, 
               user_inputs       :: [string()],
               focus_sel         :: any(),
               search_paths      ::[dir()|filename()], 
               tabwidth =8        ::integer()
              }).

-define(RULE(Before, After, Cond),
        refac_api:make_rule(Before, After, Cond)).

-define(QUOTE(Str), refac_api:quote(Str)).

-define(SPLICE(Node), refac_api:splice(Node)).

-define(COLLECT(Temp, Cond, ReturnFun, FileOrDirs), 
        refac_api:collect(Temp, Cond, ReturnFun, FileOrDirs)).

-define(COLLECT_LOC(Temp, Cond,FileOrDirs),
        begin
            Files = refac_misc:expand_files(FileOrDirs, ".erl"),
            Res=[begin
                     Ranges=refac_api:collect(Temp, Cond, refac_api:start_end_loc(_This@), [File]),
                     [{File, Range}||Range<-Ranges]
                 end||File<-Files],
           lists:append(Res)
        end).


-define(EQUAL(Node1, Node2), 
        refac_api:equal(Node1, Node2)).

-define(MATCH(Temp, Node), 
        refac_api:match(Temp, Node)).

-define(STOP_TD(Rules, FileOrDirs),
        refac_api:stop_td_search_and_transform(Rules, FileOrDirs)).

-define(FULL_TD(Rules, FileOrDirs),
        refac_api:full_td_search_and_transform(Rules, FileOrDirs)).

-define(T(Temp), Temp).
