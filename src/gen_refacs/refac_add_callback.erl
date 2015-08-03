%%%-------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela
%%% @doc
%%% Adds a callback declaration for the function with the name
%%% and arity specified.
%%% @end
%%% Created : 10 Jun 2015 by Pablo Lamela
%%%-------------------------------------------------------------------
-module(refac_add_callback).

-behaviour(gen_refac).

%% Include files
-include("../../include/wrangler.hrl").

%%%===================================================================
%% gen_refac callbacks
-export([input_par_prompts/0,select_focus/1, 
	 check_pre_cond/1, selective/0, 
	 transform/1]).

-export([add_callback/6]).

%%%===================================================================
%%% gen_refac callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prompts for parameter inputs
%%
%% @spec input_par_prompts() -> [string()]
%% @end
%%--------------------------------------------------------------------
input_par_prompts() ->
    ["Name of the function to which add callback declaration : ",
     "Arity of the function to which add callback declaration : "].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Select the focus of the refactoring.
%%
%% @spec select_focus(Args::#args{}) ->
%%                {ok, syntaxTree()} |
%%                {ok, none}
%% @end
%%--------------------------------------------------------------------
select_focus(_Args) ->
    {ok, none}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the pre-conditions of the refactoring.
%%
%% @spec check_pre_cond(Args :: #args{}) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
check_pre_cond(_Args) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Selective transformation or not.
%%
%% @spec selective() -> boolean()
%% @end
%%--------------------------------------------------------------------
selective() ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function does the actual transformation.
%%
%% @spec transform(Args::#args{}) -> 
%%            {ok, [{filename(), filename(), syntaxTree()}]} |
%%            {error, Reason}
%% @end
%%--------------------------------------------------------------------
transform(#args{current_file_name = File,
		user_inputs = [FunctionName, Arity]} = _Args) ->
    FAs = [wrangler_syntax:tuple(
	     [wrangler_syntax:atom(FunctionName),
	      wrangler_syntax:integer(list_to_integer(Arity))])],
    case collect_callbacks(File) of
	[] -> case add_to_export(File) of
		  {ok, [{_, AST}]} -> {ok, [{{File, File}, insert_attribute(FAs, AST)}]};
		  {ok, []} -> {ok,AST} = api_refac:get_ast(File),
			      {ok, [{{File, File}, insert_attribute(FAs, AST)}]}
	      end;
	_ -> add_uses(FAs, File)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

insert_attribute(FAs, AST) ->
    api_refac:insert_an_attr(
      AST, generate_callback(FAs)).

generate_callback(FAs) ->
    Template =
	lists:flatten(
	  io_lib:format("behaviour_info(callbacks) ->
                             ~s;
                         behaviour_info(_Other) ->
                             undefined.",
			[lists:flatten(?PP(wrangler_syntax:list(FAs)))])),
    ?TO_AST(Template).

add_to_export(File) ->
    F = "behaviour_info",
    A = "1",
    case api_refac:is_exported({F, A}, File) of 
        true ->
            {ok, []};
        false ->
            case collect_exports(File) of 
                [] ->
                    {ok,AST} = api_refac:get_ast(File),
                    Export=make_export_attr({F,A}),
                    NewAST=api_refac:insert_an_attr(AST,Export),
                    {ok, [{{File, File}, NewAST}]};
                Exports ->
                    Export = lists:last(Exports),
                    ?STOP_TD_TP([add_to_export_rule(Export, {F, A})], [File])
            end
    end.


add_to_export_rule(Export, FA) ->
    ?RULE(?T("Form@"),
          api_refac:add_to_export(Form@, FA),
          Form@==Export).

collect_exports(File) ->
    ?STOP_TD_TU([?COLLECT(?T("Form@"),
                          _This@,
                          api_refac:is_attribute(Form@, export))],
                [File]).

make_export_attr(FA) ->
    ?TO_AST("-export(["++format_fa(FA)++"]).").

format_fa({F,A}) ->
    lists:flatten(io_lib:format("~p/~p", [F,A])).

add_uses(Uses, File) ->
    ?STOP_TD_TP([?RULE(
		    ?T("behaviour_info(callbacks) -> Funcs@;"),
		    begin
			NewFuncs@ = wrangler_syntax:list(
				       Uses ++
				       wrangler_syntax:list_elements(Funcs@)
				       ),
		    ?TO_AST("behaviour_info(callbacks) -> NewFuncs@;",
			    wrangler_syntax:get_pos(_This@))
		    end,
		    true)],
		[File]).

collect_callbacks(File) ->
    ?FULL_TD_TU([?COLLECT(?T("behaviour_info(callbacks) -> Funcs@@;"),
			  Funcs@@, true)],
		[File]).

%%--------------------------------------------------------------------
%% @doc
%% Adds a callback declaration for the function with the name
%% and arity specified.
%% @spec add_callback(TargetFileName :: string(), FunctionName :: string(),
%%                    Arity :: string(), SearchPaths :: [string()],
%%                    Editor :: wrangler_refacs:context(),
%%                    TabWidth :: integer()) ->
%%                           {'ok', UpdatedFiles :: [string()]}
%% @end
%%--------------------------------------------------------------------
add_callback(FileName, FunName, FunArity, SearchPaths, Editor, TabWidth) ->
    Args=#args{current_file_name=FileName,
	       search_paths=SearchPaths,
	       user_inputs = [FunName, FunArity],
	       tabwidth=TabWidth},
    {ok, Res}=transform(Args),
    wrangler_write_file:write_refactored_files(Res,Editor,TabWidth,"").

