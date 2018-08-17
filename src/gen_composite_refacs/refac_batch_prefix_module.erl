%%@hidden
%%@private

%%@doc This example code tries to add a prefix, which is provided by
%% the user, to every Erlang module name in the project. The scope of 
%% the project is specified by a collection of directories.

%%@author  Huiqing Li <H.Li@kent.ac.uk>
-module(refac_batch_prefix_module).

-export([input_par_prompts/0,
         select_focus/1,
         composite_refac/1]).

-behaviour(gen_composite_refac).

-include("wrangler.hrl").

-spec input_par_prompts() -> [string()].
input_par_prompts() ->
    ["Prefix: "].
    
-spec select_focus(Args::#args{}) ->{ok, none}.    
select_focus(_Args) ->
    {ok, none}.

-spec composite_refac(Args::#args{}) -> composite_refac().
composite_refac(_Args=#args{user_inputs=[Prefix],
                            search_paths=SearchPaths}) ->
    ?non_atomic(
       ?refac_(rename_mod, 
               [{file, fun(File)-> 
                               filename:extension(File)==".erl" end},
                {generator, fun(File) ->
                                Prefix++filename:basename(File)
                            end},
                false,
                SearchPaths])).


 
