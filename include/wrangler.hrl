%%% This is an -*- Erlang -*- file.
%%%-------------------------------------------------------------------
%%% File    : wrangler.hrl
%%%-------------------------------------------------------------------

-compile({parse_transform, expand_rule}).

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
-type(syntaxTree()::any()).    %% any() should be refined.
-type(pos()::{integer(), integer()}).
-type(line()::integer()).
-type(col()::integer()).
-type(key():: attributes | errors | exports | functions | imports | module_imports | module | records | rules | warnings).
-type(moduleInfo()::[{key(), any()}]). 
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
-define(wrangler_io(__String, __Args),refac_io:format(__String, __Args)).
-else.
-define(wrangler_io(__String, __Args), ok).
-endif.

-define(RULE(Before, After, Cond),
        fun()->
                refac_api:check_collect_template(Before, '?RULE'),
                {rule, fun(_W_Node_) ->
                               _W_NewCond=fun(_W_Bind_) -> 
                                                  refac_api:make_cond(Cond, _W_Bind_)
                                          end,
                               case refac_api:match(Before, _W_Node_, _W_NewCond) of
                                   {true, _W_Bind1_} ->
                                       _This@=_W_Node_,
                                       refac_api:generate_bindings(Before, '_W_Bind1_'),
                                       _W_After=fun()-> After end(),
                                       {refac_misc:reset_pos_and_range(_W_After), true};
                                   false ->{_W_Node_, false}
                               end 
                       end,Before}
        end()).


-define(T(Template), refac_api:template(Template)).
 
-define(QUOTE(Str), refac_api:quote(Str)).

-define(SPLICE(Node), refac_api:splice(Node)).

-define(COLLECT(Temp,Collector, Cond),
        fun()->
                refac_api:check_collect_template(Temp, 'COLLECT'),
                {collector, fun(_W_File_,_W_Node_) -> 
                                    _W_NewCond_=fun(_W_Bind_) -> 
                                                        _This@=_W_Node_, 
                                                        _File@=_W_File_,
                                                        refac_api:make_cond(Cond, _W_Bind_) 
                                                end,
                                    case refac_api:match(Temp, _W_Node_, _W_NewCond_) of
                                        {true, _W_Bind1_} ->
                                            _This@=_W_Node_, 
                                            _File@=_W_File_,
                                            refac_api:generate_bindings(Temp, '_W_Bind1_'),
                                            _W_NewCollector_=fun() ->
                                                                     refac_api:expand_collector(Collector)
                                                             end,
                                            {_W_NewCollector_(), true};
                                        false ->
                                 {none, false}
                                    end
                            end, Temp}
        end()).



-define(COLLECT_LOC(Temp, Cond),
          fun()->
                refac_api:check_collect_template(Temp, 'COLLECT'),
                  {collector, fun(_W_File_,_W_Node_) -> 
                                      _W_NewCond_=fun(_W_Bind_) -> 
                                                          _This@=_W_Node_, 
                                                          _File@=_W_File_,
                                                          refac_api:make_cond(Cond, _W_Bind_) 
                                                  end,
                                      case  refac_api:match(Temp, _W_Node_, _W_NewCond_) of
                                          {true, _W_Bind1_} ->
                                              _This@=_W_Node_, 
                                              _File@=_W_File_,
                                              {{_File@, refac_api:start_end_loc(_This@)}, true};
                                          false ->
                                              {none, false}
                                      end
                              end, Temp}
          end()).

-define(EQUAL(Node1, Node2), 
        refac_api:equal(Node1, Node2)).

-define(MATCH(Temp, Node), 
        refac_api:expand_match(Temp, Node, fun(_)->true end)).

-define(STOP_TD_TP(Rules, FileOrDirs),
        refac_api:search_and_transform(Rules, FileOrDirs, stop_td_tp)).

-define(FULL_TD_TP(Rules, FileOrDirs),
        refac_api:search_and_transform(Rules, FileOrDirs, full_td_tp)).

-define(FULL_TD_TU(Collectors, FileOrDirs),
        refac_api:search_and_collect(Collectors, FileOrDirs, full_td_tu)).

-define(STOP_TD_TU(Collectors, FileOrDirs),
        refac_api:search_and_collect(Collectors, FileOrDirs, stop_td_tu)).

