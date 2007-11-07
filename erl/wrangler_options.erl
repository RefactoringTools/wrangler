-module(wrangler_options).

-export([build/1]).

-include("wrangler.hrl").

build(Opts) ->
  build_options(Opts, #options{}).

build_options([Term={OptionName,Value}|Rest], Options) ->
  case OptionName of
    files ->
      build_options(Rest, Options#options{files=Value});
    files_rec ->
      build_options(Rest, Options#options{files_rec=Value});
    core_transform ->
      build_options(Rest, Options#options{core_transform=Value});
    defines ->
      build_options(Rest, Options#options{defines=ordsets:from_list(Value)});
    from ->
      build_options(Rest, Options#options{from=Value});
    module_graph ->
      build_options(Rest, Options#options{module_graph=Value});
    init_plt ->
      build_options(Rest, Options#options{init_plt=Value});
    include_dirs ->
      build_options(Rest, Options#options{include_dirs=Value});
    output_file ->
      build_options(Rest, Options#options{output_file=Value});
    output_plt ->
      build_options(Rest, Options#options{output_plt=Value});
    plt_libs ->
      case Value of
	[] -> build_options(Rest, Options);
	_  -> build_options(Rest, Options#options{plt_libs=Value})
      end;
    supress_inline ->
      build_options(Rest, Options#options{supress_inline=Value});
    _ ->
      io:format("Bad Options:~p:\n", [Term])
  end;
build_options([Term|_Rest], _Options) ->
     io:format("Bad Options:~p:\n", [Term]);
build_options([], Options) ->
  Options.
