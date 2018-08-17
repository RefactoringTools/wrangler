%%%-------------------------------------------------------------------
%%% @author Gabriela Cunha Sampaio, Roberto Souto Maior de Barros Filho <>
%%% @copyright (C) 2014, Gabriela C. Sampaio, Roberto S. M. de Barros Filho, Simon  Thompson
%%% @doc 
%% Lists Concatenation Core - Simplifies lists concatenations. Examples:
%% <ul>
%% <li>
%% <i>"abc" ++ "def"</i> is transformed to <i>"abcdef"</i>.
%% </li>
%% <li>
%% <i>[1,2,3] ++ [4,5,6]</i> becomes <i>[1,2,3,4,5,6]</i>.
%% </li>
%% </ul>
%%@end
-module(core_lists_concat).
-include("wrangler.hrl").

%%%===================================================================
-export([rules/2]).

%%--------------------------------------------------------------------
%% @doc
%% Returns a list with a single rule for the lists concatenation simplification.
%% @spec rules(term(), term()) -> [rule()]
%% @end
%%--------------------------------------------------------------------
-spec(rules(_,_) -> [{'rule',fun(),list() | tuple()},...]).
rules(_,_) ->
    [   
     concatLists_rule()
    ]. 
%%------------------------------------------------------------------
concatLists_rule() ->
    ?RULE(
          ?T("List1@ ++ List2@"),
          concatLists(List1@, List2@),
	   ((api_refac:type(List1@) == list orelse api_refac:type(List1@) == nil orelse api_refac:type(List1@) == string)
          andalso (api_refac:type(List2@) == list orelse api_refac:type(List2@) == nil orelse api_refac:type(List2@) == string))
	).

%%--------------------------------------------------------------------
concatLists(List1, List2) ->      
                TypeList1 = api_refac:type(List1),
                TypeList2 = api_refac:type(List2),
                EmptyString1 = TypeList1 == string andalso wrangler_syntax:string_value(List1) == "",
                EmptyString2  = TypeList2 == string andalso wrangler_syntax:string_value(List2) == "",
                if 
                    (EmptyString1 andalso EmptyString2) orelse (EmptyString1 andalso TypeList2 == nil) orelse (TypeList1 == nil andalso EmptyString2) -> wrangler_syntax:nil();
		    TypeList1 == nil -> List2;
		    TypeList2 == nil -> List1;
		    TypeList1 == string andalso TypeList2 == string -> 
			String = wrangler_syntax:string_value(List1) ++ wrangler_syntax:string_value(List2),
			wrangler_syntax:string(String);
                   TypeList1 == string andalso TypeList2 == list -> 
					  String = lists:reverse(wrangler_syntax:string_value(List1)),
			                  concatStringWithList(String, List2);
                   TypeList1 == list andalso TypeList2 == string -> 
                         String = lists:reverse(wrangler_syntax:string_value(List2)),
			 NewList2 = concatStringWithList(String, wrangler_syntax:nil()),
			concatRegularLists(reverse_list(List1,wrangler_syntax:nil()), NewList2);
                   TypeList1 == list andalso TypeList2 == list ->
			concatRegularLists(reverse_list(List1,wrangler_syntax:nil()), List2);
		   true -> {error, "Invalid types!"}
                end.

%%--------------------------------------------------------------------

concatRegularLists(List1, List2) -> 
    NilList1 = api_refac:type(List1) == nil,
    if
	NilList1 -> List2;
        true -> concatRegularLists(wrangler_syntax:list_tail(List1),wrangler_syntax:cons(wrangler_syntax:list_head(List1),List2))
   end.

%%--------------------------------------------------------------------

reverse_list(List1,List2) -> 
    NilList1 = api_refac:type(List1) == nil,
    NilList2 = api_refac:type(List2) == nil,
    if
	NilList1 -> List2;
        NilList2 -> 
	    reverse_list(wrangler_syntax:list_tail(List1),wrangler_syntax:cons(wrangler_syntax:list_head(List1), wrangler_syntax:nil()));
        true -> 
           reverse_list(wrangler_syntax:list_tail(List1),wrangler_syntax:cons(wrangler_syntax:list_head(List1),List2))
   end.

%%--------------------------------------------------------------------
concatStringWithList("", List) -> List;
concatStringWithList(String, List) ->
    NewList = wrangler_syntax:cons(wrangler_syntax:integer(hd(String)), List),       
    concatStringWithList(tl(String), NewList).
    

   





