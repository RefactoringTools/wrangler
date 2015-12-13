%% @private
-module(refac_s_group).

-include("wrangler.hrl").

-export([meta_rule_set/0, simple_rule_set/0, old_apis/0]).

old_apis() ->
    [{global_group, send, 2}, {global_group, global_groups, 0}].

meta_rule_set() -> [send_meta_rule(), global_groups_meta_rule()].

simple_rule_set() -> [send_rule(), global_groups_rule()].

send_meta_rule() ->
    ?META_RULE(?T("case global_group:send(Name@,Msg@) of
                      {'badarg',Name@,Msg@} when Guard1@@ -> Body1@@;
                      Pid@ when Guard2@@ -> Body2@@
                  end"),
               begin
                   GrpName = refac_api_migration:mk_new_var("GrpName", _File@),
                   api_refac:anti_quote(refac_api_migration:mk_str("case s_group:send(hd(s_group:own_s_groups()),Name@,Msg@) of
                                                                       {'badarg',{~s,Name@,Msg@}}
                                                                           when
                                                                           Guard1@@ ->
                                                                           Body1@@;
                                                                       Pid@ when
                                                                           Guard2@@ ->
                                                                           Body2@@
                                                                   end",
                                                                   [GrpName]))
               end,
               (api_refac:free_vars(Guard1@@) --
                  (api_refac:bound_vars(Msg@) ++ api_refac:bound_vars(Name@))
                  ==
                  api_refac:free_vars(Guard1@@))).

global_groups_meta_rule() ->
    ?META_RULE(?T("case global_group:global_groups() of
                      {OwnGroupName@,GroupNames@} when Guard1@@ -> Body1@@;
                      undefined when Guard2@@ -> Body2@@
                  end"),
               begin
                   api_refac:anti_quote(refac_api_migration:mk_str("case s_group:s_groups() of
                                                                       {[OwnGroupName@],GroupNames@}
                                                                           when
                                                                           Guard1@@ ->
                                                                           Body1@@;
                                                                       undefined
                                                                           when
                                                                           Guard2@@ ->
                                                                           Body2@@
                                                                   end",
                                                                   []))
               end,
               (api_refac:free_vars(Guard1@@) --
                  (api_refac:bound_vars(GroupNames@) ++
                     api_refac:bound_vars(OwnGroupName@))
                  ==
                  api_refac:free_vars(Guard1@@))).

send_rule() ->
    ?RULE(?T("(global_group:send(Name@,Msg@))"),
          begin
              Pid = refac_api_migration:mk_new_var("Pid", _File@),
              GrpName = refac_api_migration:mk_new_var("GrpName", _File@),
              ?TO_AST(refac_api_migration:mk_str("case s_group:send(hd(s_group:own_s_groups()),Name@,Msg@) of
                                                     {'badarg',{~s,Name@,Msg@}} ->
                                                                  {'badarg',Name@,Msg@};
                                                     ~s -> ~s
                                                 end",
                                                 [GrpName, Pid, Pid]))
          end,
          true).

global_groups_rule() ->
    ?RULE(?T("(global_group:global_groups())"),
          begin
              GroupNames = refac_api_migration:mk_new_var("GroupNames",
                                                          _File@),
              OwnGroupName = refac_api_migration:mk_new_var("OwnGroupName",
                                                            _File@),
              ?TO_AST(refac_api_migration:mk_str("case s_group:s_groups() of
                                                     {[~s],~s} ->
                                                                  {~s,~s};
                                                     undefined -> undefined
                                                 end",
                                                 [OwnGroupName, GroupNames,
                                                  OwnGroupName, GroupNames]))
          end,
          true).
