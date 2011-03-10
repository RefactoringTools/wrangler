%% Copyright (c) 2010, Huiqing Li, Simon Thompson
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% Neither the name of the copyright holders nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ''AS IS''
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(wrangler_code_inspection_api).

-include("../include/wrangler.hrl").

-export([collect/3, apply_code_inspection/1]).

apply_code_inspection(Args) ->
    [ModuleName, FunctionName|_] = Args,
    M = if is_list(ModuleName) ->
                list_to_atom(ModuleName);
           true ->
                ModuleName
        end,
    F = if is_list(FunctionName) ->
                list_to_atom(FunctionName);
           true ->
                FunctionName
        end,
    try apply(M, F, [])
    catch
        throw:Error -> 
            Error;   
        _E1:_E2->
            {error, lists:flatten(io_lib:format("~p", [erlang:get_stacktrace()]))}
    end.

parse_annotate_expr(ExprStr) ->
    parse_annotate_expr(ExprStr, {1,1}).
parse_annotate_expr(ExprStr, StartLoc) ->
    case refac_scan:string(ExprStr, StartLoc) of
        {ok, Toks, _} ->
            Toks1 = case lists:last(Toks) of
                        {dot, _} -> Toks;
                        _ -> Toks++[{dot, 999}]
                    end,
            ExprOrForm=case refac_parse:parse_exprs(Toks1) of
                           {ok, Exprs} ->
                               Exprs;
                           {error, Reason} ->
                               case refac_parse:parse_form(Toks1) of 
                                   {ok, AbsForm} ->
                                       [AbsForm];
                                   _ ->
                                       throw({error, Reason})
                               end
                       end,
            ExprOrForm1 =refac_recomment:recomment_forms(ExprOrForm,[]),
            ExprOrForm2=refac_syntax:form_list_elements(
                          refac_syntax_lib:annotate_bindings(ExprOrForm1)),
            case ExprOrForm2 of 
                [EF] -> 
                    case refac_syntax:type(EF) of
                        function ->
                            Cs = refac_syntax:function_clauses(EF),
                            case Cs of 
                                [C] ->
                                    Name = refac_syntax:function_name(EF),
                                    refac_util:rewrite(C, refac_syntax:function_clause(Name, C));
                                _ -> EF
                            end;
                        _ ->
                            EF
                    end; 
                _ -> ExprOrForm2  
            end;
        _Error ->
            throw({error, "Wrangler internal error."})
    end.


collect(TemplateStr, Cond, FileOrDirs) ->
    TemplateExpr =parse_annotate_expr(TemplateStr),
    Files = refac_util:expand_files(FileOrDirs, ".erl"),
    Ranges =[collect_in_one_file(F, TemplateExpr, Cond)||F <- Files],
    display_search_results(lists:append(Ranges)).
    

collect_in_one_file(File, Template, Cond) ->
    {ok, {AST, _}} = wrangler_ast_server:parse_annotate_file(File, true, [], 8),
    do_search_matching_code(File, AST, Template, Cond).

    
do_search_matching_code(FileName, AST, Template, Cond)  when is_list(Template) ->
    do_search_matching_code_list(FileName, AST, Template, Cond);
do_search_matching_code(FileName, AST, Template, Cond) ->
    do_search_matching_code_non_list(FileName, AST, Template, Cond).


do_search_matching_code_list(FileName, AST, Template, Cond) ->
    Fun = fun(Node, Acc) ->
                  Nodes = get_expr_seqs(Node),
                  Res=generalised_unification:expr_match(Template, Nodes),
                  case Res of
                      {true, Binds} ->
                          case Cond(Binds) of
                              true ->
                                  StartEndLoc = refac_util:get_start_end_loc(Nodes),
                                  [{FileName, StartEndLoc}|Acc];
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


do_search_matching_code_non_list(FileName, AST, Template, Cond) ->
      Fun= fun(Node, Acc) ->
                   Res = generalised_unification:expr_match(Template, Node),
                 case Res of
                     {true, Binds} ->
                         case Cond(Binds) of
                             true ->
                                 StartEndLoc = refac_util:get_start_end_loc(Node),
                                 [{FileName, StartEndLoc}|Acc];
                             false ->
                                 Acc
                         end;
                     false ->
                         Acc
                 end
         end,
    refac_io:format("Template:\n~p\n", [Template]),
    case refac_syntax:type(Template) of 
        function_clause ->
            AST1=extend_function_clause(AST),
            ast_traverse_api:fold(Fun, [], AST1);
        _ -> 
            ast_traverse_api:fold(Fun, [], AST)
    end.
  

display_search_results(Ranges) ->
    case Ranges of
	[] -> 
	    ?wrangler_io("\nNo code fragment that matches the template specified has been found.\n", []),
	    {ok, Ranges};
	_ -> 
            Num = length(Ranges),
            if Num==1 ->
                    ?wrangler_io("\n~p code fragment that matches the template specified have been found. \n", [Num]);
               true ->
                    ?wrangler_io("\n~p code fragments that match the template specified have been found. \n", [Num])
            end,
	    ?wrangler_io(compose_search_result_info(Ranges), []),
	    ?wrangler_io("\n\nNOTE: Use 'M-x compilation-minor-mode' to make the result "
			 "mouse clickable if this mode is not already enabled.\n",[]),
	    ?wrangler_io("      Use 'C-c C-w e' to remove highlights!\n", []),
	    {ok, Ranges}
    end.

compose_search_result_info(Ranges) ->
    compose_search_result_info(Ranges, "").
compose_search_result_info([], Str) ->
    Str;
compose_search_result_info([{FileName, {{StartLine, StartCol}, {EndLine, EndCol}}}|Ranges], Str) ->
    Str1 =Str ++ "\n"++FileName++io_lib:format(":~p.~p-~p.~p: ", [StartLine, StartCol, EndLine, EndCol]),
    compose_search_result_info(Ranges, Str1).


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
    refac_syntax:function(Name, Cs1).


%% reverse_function_clause(AST) ->
%%      Es = refac_syntax:form_list_elements(AST),
%%      refac_syntax:form_list([case refac_syntax:type(E) of
%%                                  function->reverse_function_clause_1(E);
%%                                  _ -> E
%%                              end||E<-Es]).

%% reverse_function_clause_1(FunAST) ->
%%     Cs = refac_syntax:function_clauses(FunAST),
%%     case [C||C<-Cs, refac_syntax:type(C)==function_clause] of 
%%         [] -> FunAST;
%%         Cs1->
%%             case length(Cs)==length(Cs1) of 
%%                 true ->
%%                     {Names, Cs2} =lists:unzip([{refac_syntax:function_clause_name(C),
%%                                                 refac_syntax:function_clause(C)}||C<-Cs1]),
%%                     NameVals =[refac_syntax:atom_value(Name)||Name<-Names],
%%                     case lists:usort(NameVals) of 
%%                         [_] ->
%%                             refac_syntax:function(hd(Names), Cs2);
%%                         _ ->
%%                             erlang:error("unconsistent transformation.")
%%                     end;
%%                 false ->
%%                    erlang:error("unconsistent transformation.")
%%             end
%%     end.


%% collect_meta_apply() ->
%%     code_inspection_api:collect("erlang:apply(M@, F@, Args@)", 
%%                                 refac_syntax:type(Args@)==list andalso refac_syntax:is_atom(M@, ch),
%%                                 ["c:/cygwin/home/hl/test/ch.erl"]).


%% collect_exprs() ->
%%     code_inspection_api:collect("E1@,E2@,E3@,E4@", true, ["c:/cygwin/home/hl/test/ch.erl"]).


%% collect_exprs1() ->
%%     code_inspection_api:collect("E@@, E1@=E2@, E1@", true, ["c:/cygwin/home/hl/test/ch.erl"]).


%% collect_fun() ->
%%     code_inspection_api:collect("foo(Args@) -> Es@@", length(Es@@)>2, ["c:/cygwin/home/hl/test/ch.erl"]).


    
%% collect_exprs(File) ->
%%     collect("E1@+E2@+E3@", [File]).
    
 

%% collect_bad_cases(File) ->
%%     collect("case E@ of 
%%                 E1@ ->
%%                     E1@;
%%                 E2@ ->
%%                     E2@
%%             end", [File]).


%% collect_bad_cases(File) ->
%%     collect("Es@@,
%%              E@=E1@,
%%              E@",
%%             [File]).

%% foo(M@) ->
%%     Es@,
%%     E@,
%%     Es1@.

%% F(_M@) ->
%%     Es@,
%%     X=Exp,
%%     E[Exp/X],
%%     Es1@.


%% NEED A FORMAL SEMANTICS!!!
%% The template should represent each a single node or a list of nodes. 
%% only qualified function names are allowed. 
%% both free and bound variables can be be used in the template. Binding 
%% structure for bound variables is checked, but not free variables.
%% the template should be parsable by Erlang parser.
%% use VarName@ to represent meta variable, Varname@@ to represent a variable 
%% which is a sequence of elements.
%% a variables not ending with @ represents an object variable.
%% Question: do I need meta atom? In Erlang, not erlang node can be 
%% replaced by a variable.
%% variables in the format of '_@' or '_@@' means that there wount be binds for these variables.
%% the template should represent a single form; function clause; an expression/pattern, or an expressio
%%  pattern sequence seperated by comma.
%% TODO: try more refactorings, and summarise the limiations, what can be done, and what cannot!!!
