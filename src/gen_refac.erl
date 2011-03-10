-module(gen_refac).

-export([run_refac/2, input_pars/1]).

-export([parse_annotate_expr/1, subst/2]).

-compile(export_all).

-include("../include/gen_refac.hrl").

-spec(select_focus(Module::module(), Args::[term()]) ->
             {ok, term()} | {error, term()}).
select_focus(Module, Args) ->
    apply(Module, select_focus, [Args]).

-spec(pre_cond_check(Module::module(), Args::[term()]) ->
             true | {error, term()}).
pre_cond_check(Module, Args) ->
    apply(Module, pre_cond_check, [Args]).

-spec(transform(Module::module(), Args::[term()]) ->
             {ok, [{filename(), filename(), syntaxTree()}]} |
             {error, term()}).
transform(Module, Args) ->
    apply(Module, transform, [Args]).

-spec(run_refac(Module::module(), Args::[term()]) ->
             {ok, string()} | {error, term()}).
run_refac(Module0, Args0=[CurFileName, [Line,Col], [[StartLine, StartCol], [EndLn, EndCol]], UserInputs, 
                          SearchPaths, TabWidth])->
    ?wrangler_io("\nCMD: ~p:run_refac(~p,~p).\n",
		 [?MODULE, Module0, Args0]),
    Module = if is_list(Module0) ->
                     list_to_atom(Module0);
                 true ->
                     Module0
             end,
    Args=#args{current_file_name=CurFileName,
               cursor_pos = {Line, Col},
               highlight_range = {{StartLine, StartCol}, {EndLn, EndCol}}, 
               user_inputs = UserInputs,
               search_paths = SearchPaths, 
               tabwidth = TabWidth},
    case select_focus(Module, Args) of
        {ok, Sel} ->
            Args1 = Args#args{focus_sel=Sel},
            case pre_cond_check(Module, Args1) of
                ok -> 
                    case transform(Module,Args1) of
                        {ok, Res} ->
                            refac_write_file:write_refactored_files(
                              Res, 'emacs', TabWidth, "");
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


input_pars(CallBackMod) ->
    Res =input_pars_1(CallBackMod),
    {ok, Res}.

input_pars_1(CallBackMod) when is_atom(CallBackMod)->
    erlang:apply(CallBackMod, input_pars, []);
input_pars_1(CallBackMod) when is_list(CallBackMod)->
    erlang:apply(list_to_atom(CallBackMod), input_pars, []);
input_pars_1(_) ->
    throw:error(badarg).
    
%% a list of expressions, a single function, a single function clause.
parse_annotate_expr("") ->
    refac_syntax:empty_node();
parse_annotate_expr(ExprStr) ->
    parse_annotate_expr(ExprStr, {1,1}).
parse_annotate_expr(ExprStr, StartLoc) when is_integer(StartLoc) ->
    parse_annotate_expr(ExprStr, {StartLoc, 1});
parse_annotate_expr(ExprStr, StartLoc) when is_tuple(StartLoc) ->
    case refac_scan:string(ExprStr, StartLoc) of
        {ok, Toks, _} ->
            refac_io:format("ExprStr:\n~p\n", [ExprStr]),
            [T|Ts] = lists:reverse(Toks),
            Toks1 = case T of 
                        {dot, _} -> Toks;
                        {';',_} -> lists:reverse([{dot, 999}|Ts]);
                        _ -> Toks++[{dot, 999}]
                    end,
            Toks2 = refac_epp_dodger:scan_macros(Toks1,[]),
            case refac_parse:parse_form(Toks2) of 
                {ok, AbsForm} ->
                    case refac_syntax:type(AbsForm) of 
                        function ->
                            Form1 =refac_epp_dodger:fix_pos_in_form(Toks, AbsForm),
                            Form2 =  refac_syntax_lib:annotate_bindings(Form1),
                            Cs = refac_syntax:function_clauses(Form2),
                            case {Cs, T} of 
                                {[C], {';',_L}} ->
                                    Name = refac_syntax:function_name(Form2),
                                    refac_util:rewrite(C, refac_syntax:function_clause(Name, C));        
                                _ ->
                                    Form2
                            end;
                        _ ->
                            refac_epp_dodger:fix_pos_in_form(Toks, AbsForm)
                    end;
                {error, Reason} ->
                    case refac_parse:parse_exprs(Toks2) of
                        {ok, Exprs} ->
                            Exprs1 =refac_epp_dodger:rewrite_list(Exprs),
                            Exprs2 = make_tree({block, StartLoc, Exprs1}),
                            Exprs3=refac_syntax_lib:annotate_bindings(Exprs2),
                            Exprs4 =refac_syntax:block_expr_body(Exprs3),
                            case Exprs4 of 
                                [E] -> E;
                                _ -> Exprs4
                            end;
                        {error, Reason} ->
                            throw({error, Reason})
                    end
            end;
        {error, ErrInfo, ErrLoc} ->
            throw({error, {ErrInfo, ErrLoc}})
    end.


make_tree(Tree) ->
    case refac_syntax:subtrees(Tree) of 
        [] ->
           Tree;
        Gs ->
            Gs1 = [[make_tree(T) || T <- G] || G <- Gs],
            refac_syntax:update_tree(Tree, Gs1)
    end.


subst(Expr, Subst) when is_list(Expr) ->
    [subst(E, Subst)||E<-Expr];
  
subst(Expr, Subst) ->
    {Expr1, _} =ast_traverse_api:stop_tdTP(fun do_subst/2, Expr, Subst),
    remove_fake_begin_end(Expr1).
    %% refac_util:reset_ann_and_pos(remove_fake_begin_end(Expr1)).

do_subst(Node, Subst) ->
    case refac_syntax:type(Node) of
	variable ->
            VarName = refac_syntax:variable_name(Node),
            case lists:keysearch(VarName, 1, Subst) of
                {value, {VarName, Expr}} ->
                    case is_meta_list_variable(VarName) andalso
                        is_list(Expr) of 
                        true -> 
                            case Expr of 
                                [E] ->
                                    {refac_util:reset_ann_and_pos(E), true};
                                _ ->
                                    E1=refac_syntax:add_ann(
                                         {fake_block_expr, true},
                                         refac_util:reset_ann_and_pos(
                                           refac_syntax:block_expr(Expr))),
                                    {E1, true}
                            end;
                        false ->
                            E1=refac_util:reset_ann_and_pos(Expr),
                            {E1,  true}
                    end;
                _ -> {Node, false}
            end;
        atom ->
            AtomValue = refac_syntax:atom_value(Node),
            case is_meta_atom_name(AtomValue) of 
                true ->
                    case lists:keysearch(AtomValue, 1, Subst) of
                        {value, {AtomValue, Expr}} ->
                            {refac_util:reset_pos(Expr), true};
                        false ->
                            {Node, false} %% TODO: SHOULD ISSUE AN ERROR MSG HERE!!!
                    end;
                _ ->
                    {Node, false}
            end;
	_ -> {Node, false}
    end.


remove_fake_begin_end(Node) ->
    case refac_syntax:subtrees(Node) of
        [] -> Node;
        Gs ->
            Gs1 = [[remove_fake_begin_end(T)||T<-remove_fake_begin_end_1(G)] || G <- Gs],
            Node2 = refac_syntax:make_tree(refac_syntax:type(Node), Gs1),
            refac_syntax:copy_attrs(Node, Node2)
    end.


remove_fake_begin_end_1(Node)->
    lists:append([remove_fake_begin_end_2(N)||N<-Node]).

remove_fake_begin_end_2(Node) when is_list(Node) ->
    [Node];
remove_fake_begin_end_2(Node) ->
    case refac_syntax:type(Node) of
        block_expr ->
            Ann = refac_syntax:get_ann(Node),
            case lists:keysearch(fake_block_expr, 1,Ann) of
                {value, _} ->
                    refac_syntax:block_expr_body(Node);
                false ->
                    [Node]
            end;
        _ ->
            [Node]
    end.
                                   
is_meta_list_variable(VarName) ->
    lists:prefix("@@", lists:reverse(atom_to_list(VarName))).
       
is_meta_atom_name(AtomName) ->
    AtomName1 = atom_to_list(AtomName),
    refac_util:is_fun_name(AtomName1) andalso
    lists:prefix("@", lists:reverse(AtomName1)).

extend_function_clause(AST) ->
    Es = refac_syntax:form_list_elements(AST),
    refac_syntax:form_list([case refac_syntax:type(E) of
                                function->extend_function_clause_1(E);
                                _ -> E
                            end||E<-Es]).

extend_function_clause_1(FunAST) ->
    Name = refac_syntax:function_name(FunAST),
    Cs = refac_syntax:function_clauses(FunAST),
    Cs1= [refac_util:rewrite(C,refac_syntax:function_clause(Name, C))||C<-Cs],
    refac_util:rewrite(FunAST, refac_syntax:function(Name, Cs1)).

reverse_function_clause(AST) ->
     Es = refac_syntax:form_list_elements(AST),
     refac_syntax:form_list([case refac_syntax:type(E) of
                                 function->
                                     E1=reverse_function_clause_1(E),
                                     E1;
                                 _ -> E
                             end||E<-Es]).

reverse_function_clause_1(FunAST) ->
    FunName = refac_syntax:function_name(FunAST),
    Cs = refac_syntax:function_clauses(FunAST),
    case [C||C<-Cs, refac_syntax:type(C)==function_clause] of 
        [] -> FunAST;
        Cs1->
            case length(Cs)==length(Cs1) of 
                true ->
                    {Names, Cs2} =lists:unzip([{refac_syntax:function_clause_name(C),
                                                refac_syntax:function_clause(C)}||C<-Cs1]),
                    NameVals =[refac_syntax:atom_value(Name)||Name<-Names],
                    case lists:usort(NameVals) of 
                        [_] ->
                            refac_util:rewrite(FunAST, 
                                               refac_syntax:function(
                                                 refac_util:rewrite(FunName, hd(Names)), Cs2));
                        _ ->
                            erlang:error("unconsistent transformation.")
                    end;
                false ->
                   erlang:error("unconsistent transformation.")
            end
    end.

%% -spec(search_and_transform([rules()], [filename()|dir()]|syntaxTree()) ->
%%              [{{filename(),filename()}, syntaxTree()}]|syntaxTree()).
search_and_transform(Rules, Input={_File, _AST}) ->
    search_and_transform_1(Rules, Input);
search_and_transform(Rules, Input) ->
    case is_list(Input) of 
        true ->
            search_and_transform_2(Rules, Input);
        false ->
            throw({error, "Wrangler internal error in "
                   "function search_and_transform/2"})
    end.
    

search_and_transform_1(Rules, {File, AST}) ->
    ParsedBeforeAfterConds=[{parse_annotate_expr(R#rule.template_before),
                             R#rule.template_after,
                             R#rule.condition}|| R<-Rules],
    refac_io:format("Data:\n~p\n", [ParsedBeforeAfterConds]),
    Fun = fun(Node, _) ->
                  Res = try_expr_match(ParsedBeforeAfterConds, Node),
                  case Res of
                      {true, NewExprAfter} ->
                          {refac_util:rewrite(Node,NewExprAfter), true};
                      false ->
                          {Node, false}
                  end
          end,
    AST1=element(1, full_tdTP(Fun, AST, {})),
    {ok, [{{File, File}, AST1}]}.
   
search_and_transform_2(Rules, FileOrDirs)  ->
    Files = refac_util:expand_files(FileOrDirs, ".erl"),
    Res=lists:append([search_and_transform_3(Rules, File)||File<-Files]),
    {ok, Res}.

search_and_transform_3(Rules, File)->
    ParsedBeforeAfterConds=[{parse_annotate_expr(R#rule.template_before),
                             R#rule.template_after,
                             R#rule.condition}|| R<-Rules],
    Fun = fun(Node, _) ->
                  Res = try_expr_match(ParsedBeforeAfterConds, Node),
                  case Res of
                      {true, NewExprAfter} ->
                          
                          {NewExprAfter, true};
                      false ->
                          {Node, false}
                  end
          end,
    {ok, {AST, _}} = wrangler_ast_server:parse_annotate_file(File, true, [], 8),
    AST0 = extend_function_clause(AST),
    {AST1, Changed} =full_tdTP(Fun, AST0, {}),
    if Changed -> 
            AST2= reverse_function_clause(AST1),
            [{{File, File}, AST2}];
       true ->
             []
    end.

full_tdTP(Fun, Node, Others) ->
    {Node1, C} =full_tdTP_1(Fun, Node, Others),
    {remove_fake_begin_end(Node1),C}.
        
full_tdTP_1(Fun, Node, Others) ->
    Gs = refac_syntax:subtrees(Node),
    case Gs of 
        [] ->
            {Node1, Changed} =Fun(Node, Others),
            if  is_list(Node1) -> 
                {make_fake_block_expr(Node1), true};
                true ->
                    {Node1, Changed}
            end;
        _ ->
            Gs1 = [[{full_tdTP_1(Fun, T, Others),C}||T<-G1]||
                      G<-Gs,{G1, C}<-[Fun(G, Others)]],
            Gs2 = [[N || {{N, _B}, _C} <- G] || G <- Gs1],
            G = [[B or C|| {{_N, B}, C} <- G] || G <- Gs1],
            Node0 =refac_util:rewrite(
                     Node, refac_syntax:make_tree(refac_syntax:type(Node), Gs2)),
            {Node1, Changed} = Fun(Node0, Others),
            {Node2, C}= 
                if is_list(Node1) ->
                        {make_fake_block_expr(Node1), true};
                   true ->
                        {Node1, Changed}
                end,
            {Node2, C or
             lists:member(true, lists:flatten(G))}
    end.

   
make_fake_block_expr(Es) ->
    refac_syntax:add_ann({fake_block_expr, true},
                         refac_syntax:block_expr(Es)).

try_expr_match([], _Node) ->false;
try_expr_match([{BeforeExpr, AfterExpr, Cond}|T], Node) 
  when is_list(BeforeExpr) andalso is_list(Node) ->
    try_expr_match_2([{BeforeExpr, AfterExpr, Cond}|T], Node,1);
    
try_expr_match([{BeforeExpr, AfterExpr, Cond}|T], Node) when 
      not is_list(BeforeExpr) andalso not is_list(Node)->
    try_expr_match_1([{BeforeExpr, AfterExpr, Cond}|T], Node);
try_expr_match([_|T], Node) ->
    try_expr_match(T, Node).

try_expr_match_1([{BeforeExpr, AfterExpr, Cond}|T], Node) ->
    case generalised_unification:expr_match(BeforeExpr, Node) of 
        {true, Binds} ->
            Binds1 = convert_meta_atom_to_meta_var(Binds),
            Binds2 = [{'_This@', Node}|Binds1],
            case  Cond(Binds2) of 
                true ->
                    NewExprAfter =refac_util:rewrite(Node, AfterExpr(Binds2)),
                    {true, NewExprAfter};
                false ->
                    try_expr_match(T, Node)
            end;
        false ->
            try_expr_match(T, Node)
    end.

try_expr_match_2([{BeforeExpr, AfterExpr, Cond}|T], NodeList, Index) ->
    Len1 = length(BeforeExpr),
    Len2 = length(NodeList),
    case Len1 =< Len2 of
        true ->
            Exprs = lists:sublist(NodeList, Index, Len1),
            case generalised_unification:expr_match(BeforeExpr,Exprs) of 
                {true, Binds} ->
                    Binds1 = convert_meta_atom_to_meta_var(Binds),
                    Binds2 = [{'_This@', Exprs}|Binds1],
                    case Cond(Binds2) of 
                        true ->
                            NewAfterExpr = AfterExpr(Binds),
                            NewAfterExpr1 = case NewAfterExpr of
                                                [E|Es]->
                                                    [refac_util:rewrite(hd(Exprs), E)|Es];
                                                _ ->
                                                    [refac_util:rewrite(hd(Exprs),NewAfterExpr)]
                                            end,
                            NewNodeList =  lists:sublist(NodeList, Index-1) ++
                                NewAfterExpr1 ++  lists:nthtail(Index+Len1-1, NodeList),
                            {true, NewNodeList};
                        false ->
                            if Index < Len2 ->
                                    try_expr_match_2([{BeforeExpr, AfterExpr, Cond}|T], NodeList, Index+1);
                               true ->
                                    try_expr_match(T, NodeList)
                            end
                    end;
                _ ->
                    if Index < Len2 ->
                            try_expr_match_2([{BeforeExpr, AfterExpr, Cond}|T], NodeList, Index+1);
                       true ->
                            try_expr_match(T, NodeList)
                    end
            end;
         false->
            try_expr_match(T, NodeList)
    end.
            
    
convert_meta_atom_to_meta_var(Binds) ->
    convert_meta_atom_to_meta_var_1(Binds, []).

convert_meta_atom_to_meta_var_1([], Acc) ->
    lists:reverse(Acc);
convert_meta_atom_to_meta_var_1([{Name, Node}|T], Acc) ->
    case is_meta_atom_name(Name) of 
        true ->
            Name1 = list_to_atom(string:to_upper(atom_to_list(Name))), 
            convert_meta_atom_to_meta_var_1(T, [{Name1, Node}, {Name, Node}|Acc]);
        false ->
            convert_meta_atom_to_meta_var_1(T, [{Name, Node}|Acc])
    end.
    

make_rule(Before, After, Cond) ->
    #rule{template_before=Before, 
          template_after=After,
          condition=Cond}.
   

collect(TemplateStr, Cond, ReturnFun, FileOrDirs) ->
    TemplateExpr =parse_annotate_expr(TemplateStr),
    Files = refac_util:expand_files(FileOrDirs, ".erl"),
    Res=[collect_in_one_file(F, {TemplateExpr, Cond, ReturnFun})||F <- Files],
    lists:append(Res).
 
collect_in_one_file(File, {Template, Cond, ReturnFun}) ->
    {ok, {AST, _}} = wrangler_ast_server:parse_annotate_file(File, true, [], 8),
    do_search_matching_code(File, AST, {Template, Cond, ReturnFun}).

  

%% THE FOLLOWING WILL BE REFACTORED!!!.
do_search_matching_code(FileName, AST, {Template, Cond, ReturnFun}) 
  when is_list(Template) ->
    do_search_matching_code_list(FileName, AST, {Template, Cond, ReturnFun});
do_search_matching_code(FileName, AST, {Template, Cond, ReturnFun}) ->
    do_search_matching_code_non_list(FileName, AST, {Template, Cond, ReturnFun}).


do_search_matching_code_list(_FileName, AST, {Template, Cond, ReturnFun}) ->
    Fun = fun(Node, Acc) ->
                  Nodes = get_expr_seqs(Node),
                  Res=generalised_unification:expr_match(Template, Nodes),
                  case Res of
                      {true, Binds} ->
                          case Cond(Binds) of
                              true ->
                                  [ReturnFun(Binds)|Acc];
                              false ->
                                  Acc
                          end;
                      false ->
                          Acc
                  end
          end,
    ast_traverse_api:fold(Fun, [], AST).
  

get_expr_seqs(T) ->
    case refac_syntax:type(T) of
	clause ->
	    refac_syntax:clause_body(T);
	block_expr ->
	    refac_syntax:block_expr_body(T);
	try_expr ->
	    refac_syntax:try_expr_body(T);
	_ -> []
    end.


do_search_matching_code_non_list(_FileName, AST, {Template, Cond, ReturnFun}) ->
    Fun= fun(Node, Acc) ->
                 Res = generalised_unification:expr_match(Template, Node),
                 case Res of
                     {true, Binds} ->
                         case Cond(Binds) of
                             true ->
                                 [ReturnFun(Binds)|Acc];
                             false ->
                                 Acc
                         end;
                     false ->
                         Acc
                 end
         end,
    case refac_syntax:type(Template) of 
        function_clause ->
            AST1=extend_function_clause(AST),
            ast_traverse_api:fold(Fun, [], AST1);
        _ -> 
            ast_traverse_api:fold(Fun, [], AST)
    end.
  
