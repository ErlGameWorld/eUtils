-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(GET_STACK(Stacktrace), Stacktrace).
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(GET_STACK(_), erlang:get_stacktrace()).
-endif.

%% 追溯打印函数调用流程
-define(PRINT_STACK(Tag),
   Stack =
      try
         throw(0)
      catch
         ?EXCEPTION(_Class, _Reason, Stacktrace) ->
            ?GET_STACK(Stacktrace)
      end,
   io:format("tarce tag:~p cur Stacktrace:", [Tag]),
   io:format(utParseStack:parseStack(Stack))
).