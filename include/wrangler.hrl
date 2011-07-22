%%% This is an -*- Erlang -*- file.
%%%-------------------------------------------------------------------
%%% File    : wrangler.hrl
%%%-------------------------------------------------------------------

-compile({parse_transform, wrangler_expand_rule}).

-record(rule, {template_before,
               template_after,
               condition}).

-record(args, {current_file_name :: filename(), 
               cursor_pos        :: pos(), 
               highlight_range   :: {pos(), pos()}, 
               user_inputs       :: [string()],
               focus_sel         :: any(),
               selective         :: boolean(),
               search_paths      ::[dir()|filename()], 
               tabwidth =8        ::integer()
              }).

-type args() :: #args{}.
-type(filename()::string()).
-type(modulename()::atom()).
-type(functionname()::atom()).
-type(functionarity()::integer()).
-type(dir()::string()).
-type(syntaxTree() :: {tree, any(), any(), any()}| {wrapper, any(), any(), any()}).
-type(pos()::{integer(), integer()}).
-type(line()::integer()).
-type(col()::integer()).
-type(key():: attributes | errors | exports | functions | imports | module_imports | module | records | rules | warnings).
-type(module_info()::[{key(), any()}]). 
-type(anyterm()::any()).
-type(editor()::emacs|eclipse).
-type(whitespace() :: '\t' | '\n' | ' ').
-type(token() :: {'var', pos(), atom()} | {'integer', pos(), integer()} | {'string', pos(), string()}
	       | {'float', pos(), float()} | {'char', pos(), char()}
	       | {'atom', pos(), atom()} | {atom(), pos()}
	       | {'whitespace', pos(), whitespace()} | {'comment', pos(), string()}).

-type(scc_order()::[[{{atom(), atom(), integer()}, syntaxTree()}]]).
-type(callercallee()::[{{modulename(), functionname(), functionarity()}, [{modulename(), functionname(), functionarity()}]}]).
-type(external_calls()::[{atom(), atom(), integer()}]).
-record(callgraph, {'callercallee', 'scc_order', 'external_calls'}).
      
-define(EMACS, true).

-ifdef(EMACS).
-define(wrangler_io(__String, __Args),wrangler_io:format(__String, __Args)).
-else.
-define(wrangler_io(__String, __Args), ok).
-endif.

-define(RULE(Before, After, Cond),
        fun()->
                api_refac:check_collect_template(Before, '?RULE'),
                {rule, fun(_W_Node_) ->
                               _W_NewCond=fun(_W_Bind_) -> 
                                                  api_refac:make_cond(Cond, _W_Bind_)
                                          end,
                               case api_refac:match(Before, _W_Node_, _W_NewCond) of
                                   {true, _W_Bind1_} ->
                                       _This@=_W_Node_,
                                       api_refac:generate_bindings(Before, '_W_Bind1_'),
                                       _W_After=fun()-> After end(),
                                       {wrangler_misc:reset_pos_and_range(_W_After), true};
                                   false ->{_W_Node_, false}
                               end 
                       end,Before}
        end()).


-define(T(Template), api_refac:template(Template)).
 
-define(QUOTE(Str), api_refac:quote(Str)).

-define(SPLICE(Node), api_refac:splice(Node)).

-define(COLLECT(Temp,Collector, Cond),
        fun()->
                api_refac:check_collect_template(Temp, 'COLLECT'),
                {collector, fun(_W_File_,_W_Node_) -> 
                                    _W_NewCond_=fun(_W_Bind_) -> 
                                                        _This@=_W_Node_, 
                                                        _File@=_W_File_,
                                                        api_refac:make_cond(Cond, _W_Bind_)
                                                end,
                                    case api_refac:match(Temp, _W_Node_, _W_NewCond_) of
                                        {true, _W_Bind1_} ->
                                            _This@=_W_Node_, 
                                            _File@=_W_File_,
                                            api_refac:generate_bindings(Temp, '_W_Bind1_'),
                                            _W_NewCollector_=fun() ->
                                                                     api_refac:expand_collector(Collector)
                                                             end,
                                            {_W_NewCollector_(), true};
                                        false ->
                                 {none, false}
                                    end
                            end, Temp}
        end()).



-define(COLLECT_LOC(Temp, Cond),
          fun()->
                api_refac:check_collect_template(Temp, 'COLLECT'),
                {collector, fun(_W_File_,_W_Node_) ->
                                    _W_NewCond_=fun(_W_Bind_) ->
                                                        _This@=_W_Node_,
                                                        _File@=_W_File_,
                                                        api_refac:make_cond(Cond, _W_Bind_)
                                                end,
                                    case  api_refac:match(Temp, _W_Node_, _W_NewCond_) of
                                        {true, _W_Bind1_} ->
                                            _This@=_W_Node_,
                                            _File@=_W_File_,
                                            {{_File@, api_refac:start_end_loc(_This@)}, true};
                                        false ->
                                            {none, false}
                                    end
                            end, Temp}
          end()).

-define(EQUAL(Node1, Node2), 
        api_refac:equal(Node1, Node2)).

-define(MATCH(Temp, Node), 
        api_refac:expand_match(Temp, Node, fun(_) -> true end)).

-define(STOP_TD_TP(Rules, FileOrDirs),
        api_refac:search_and_transform(Rules, FileOrDirs, stop_td_tp)).

-define(FULL_TD_TP(Rules, FileOrDirs),
        api_refac:search_and_transform(Rules, FileOrDirs, full_td_tp)).

-define(FULL_TD_TU(Collectors, FileOrDirs),
        api_refac:search_and_collect(Collectors, FileOrDirs, full_td_tu)).

-define(STOP_TD_TU(Collectors, FileOrDirs),
        api_refac:search_and_collect(Collectors, FileOrDirs, stop_td_tu)).

