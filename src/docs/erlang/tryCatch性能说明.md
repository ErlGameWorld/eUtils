# otp
    otp24

## no catch
    utTc:ts(10, utTryCatchCase, loopTest, [1000000, testNoCatch, normal, true]).
    SumTime:  630998000(ns)   0.630998(s) 
    AvgTime: 63099800.0(ns)     0.0631(s)

## catch
###catch nomal
    utTc:ts(10, utTryCatchCase, loopTest, [1000000, testCatch, normal, true]). MaxTime:   74246000(ns)   0.074246(s)
    SumTime:  725893000(ns)   0.725893(s)
    AvgTime: 72589300.0(ns)   0.072589(s)

###catch exit
    utTc:ts(10, utTryCatchCase, loopTest, [1000000, testCatch, exit, true]).
    SumTime: 1264673000(ns)   1.264673(s)
    AvgTime: 126467300.(ns)   0.126467(s)

###catch error
    utTc:ts(10, utTryCatchCase, loopTest, [1000000, testCatch, error, true]).
    SumTime: 1414449300(ns)  14.144493(s)
    AvgTime: 1414449300(ns)   1.414449(s)

###catch thorw   
    utTc:ts(10, utTryCatchCase, loopTest, [1000000, testCatch, throw, true]).
    SumTime: 1229429000(ns)   1.229429(s)
    AvgTime: 122942900.(ns)   0.122943(s)

## try catch(不匹配starace)
###catch nomal
	utTc:ts(10, utTryCatchCase, loopTest, [1000000, testTryCatch, nomal, true]).
    SumTime:  655993000(ns)   0.655993(s)
    AvgTime: 65599300.0(ns)   0.065599(s)
    
###catch exit
    utTc:ts(10, utTryCatchCase, loopTest, [1000000, testTryCatch, exit, true]).
    SumTime: 1479058000(ns)   1.479058(s)
    AvgTime: 147905800.(ns)   0.147906(s)


###catch error
    utTc:ts(10, utTryCatchCase, loopTest, [1000000, testTryCatch, error, true]).
    SumTime: 1290286000(ns)   1.290286(s)
    AvgTime: 129028600.(ns)   0.129029(s)

###catch throw
    utTc:ts(10, utTryCatchCase, loopTest, [1000000, testTryCatch, throw, true]).
    SumTime: 1385688000(ns)   1.385688(s)
    AvgTime: 138568800.(ns)   0.138569(s)


## try catch(匹配starace)
###catch nomal
    utTc:ts(10, utTryCatchCase, loopTest, [1000000, testTryCatch2, normal, true]).
    SumTime:  663833000(ns)   0.663833(s)
    AvgTime: 66383300.0(ns)   0.066383(s)



###catch exit
    utTc:ts(10, utTryCatchCase, loopTest, [1000000, testTryCatch2, exit, true]).
    SumTime: 1385211400(ns)  13.852114(s)
    AvgTime: 1385211400(ns)   1.385211(s)

###catch error
    utTc:ts(10, utTryCatchCase, loopTest, [1000000, testTryCatch2, error, true]).
    SumTime: 1370759800(ns)  13.707598(s)
    AvgTime: 1370759800(ns)    1.37076(s)

###catch throw
    utTc:ts(10, utTryCatchCase, loopTest, [1000000, testTryCatch2, throw, true]).
    SumTime: 1386053800(ns)  13.860538(s)
    AvgTime: 1386053800(ns)   1.386054(s)



