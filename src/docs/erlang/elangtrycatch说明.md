# catch
    catch Expr
    除非评估期间发生异常，否则返回Expr的值。在这种情况下，将捕获异常。
    对于类错误（即运行时错误）的异常，将返回 {'EXIT'，{Reason，Stack}}。
    对于类exit的异常，即返回称为exit（Term）， {'EXIT'，Term}的代码。
    对于类throw的异常（即称为throw（Term）的代码）， 将返回Term。
    原因取决于发生的错误的类型， 堆栈是最近的函数调用的堆栈，
    
    catch error会返回堆栈 性能低
    catch exit/throw 不会返回堆栈 但是性能比普通函数调用低几倍
    
# try catch 
    匹配格式为Clcass:Reason:Strace 并且 使用了Strace的时候 会返回堆栈 性能 跟 catch error 差不多
    如果不匹配strace或者匹配了不使用Strace, try catch exit error throw 性能跟catch exit/throw差不多, 无太大差别
    
    
    
    
        