
=== How to generate Edoc files ====

Runs EDoc on a given set of source files:
  files(Files, Options) -> ok

Eshell V10.7  (abort with ^G)
1> pwd().
.../wrangler/src
ok
2> Files = ["api_refac.erl", "api_wrangler.erl"].
["api_refac.erl","api_wrangler.erl"]
3> edoc:files(Files, [{dir, "../docs/doc"}]). %% the 'dir' option specifies the output directory for the generated files.
ok


For more usage of Edoc, please check:
https://www.erlang.org/doc/apps/edoc
