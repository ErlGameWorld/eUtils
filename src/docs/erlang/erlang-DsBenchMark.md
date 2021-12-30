otp24.1.3
DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
PdDs              8     174.86ns      76.42ns       100.03ns     125.04ns     161.81ns       noSize
PdDs             16     155.88ns      59.64ns       237.20ns     101.19ns     109.85ns       noSize
PdDs             32     240.85ns      88.53ns       177.81ns     105.12ns      69.74ns       noSize
PdDs             64     323.50ns      58.59ns       121.79ns     173.61ns      73.90ns       noSize
PdDs            128     324.56ns      61.53ns       172.04ns     182.24ns      79.11ns       noSize
PdDs            256     348.28ns      96.03ns       182.09ns     184.82ns      80.24ns       noSize
PdDs            516     385.30ns      63.74ns       141.22ns     179.90ns      77.89ns       noSize
PdDs           1024     331.42ns     110.79ns       131.86ns     171.81ns      78.06ns       noSize
PdDs           2048     333.78ns      65.49ns       173.49ns     170.01ns      77.24ns       noSize
PdDs           4096     297.02ns     110.97ns       234.86ns     153.04ns      81.35ns       noSize
PdDs           8192     355.80ns      69.82ns       183.58ns     190.96ns      83.97ns       noSize
PdDs          16384     317.57ns     129.21ns       229.69ns     183.09ns      89.70ns       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
TupleDs           8      66.28ns      16.50ns        50.09ns      16.13ns   notSupport         1056
TupleDs          16     124.75ns       9.88ns       120.56ns       9.96ns   notSupport         2096
TupleDs          32     133.34ns       6.45ns       103.93ns       6.90ns   notSupport         4176
TupleDs          64     139.76ns       5.12ns       123.60ns       5.34ns   notSupport         8336
TupleDs         128     161.93ns       4.45ns       180.59ns       4.42ns   notSupport        16656
TupleDs         256     300.42ns       3.87ns       190.35ns       3.83ns   notSupport        33296
TupleDs         516     478.73ns       3.57ns       403.75ns       3.63ns   notSupport        67096
TupleDs        1024     736.33ns       3.47ns       903.67ns       3.45ns   notSupport       133136
TupleDs        2048    1564.03ns       3.31ns      1909.03ns       3.35ns   notSupport       266256
TupleDs        4096    2564.94ns       3.37ns      3119.20ns       3.55ns   notSupport       532496
TupleDs        8192    6189.21ns       3.35ns      7540.96ns       3.46ns   notSupport      1064976
TupleDs       16384   10397.99ns       3.38ns     12214.43ns       3.42ns   notSupport      2129936

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
ListsDs           8      60.61ns     108.06ns       342.79ns      14.25ns     629.66ns         1528
ListsDs          16      47.04ns     120.67ns       370.13ns       7.76ns     318.27ns         3048
ListsDs          32      69.41ns     233.06ns       428.70ns       5.34ns     402.52ns         6088
ListsDs          64      98.30ns     362.87ns       710.63ns       4.18ns     590.85ns        12168
ListsDs         128     165.16ns     796.79ns      1307.86ns       4.21ns    1097.41ns        24328
ListsDs         256     154.67ns    1207.55ns      2304.16ns       5.22ns    2066.74ns        48648
ListsDs         516     145.19ns    2395.41ns      4277.20ns       5.17ns    4256.06ns        98048
ListsDs        1024     117.30ns    4894.58ns      8671.82ns       5.34ns    8460.23ns       194568
ListsDs        2048     116.47ns    9865.03ns     17416.33ns       5.70ns   17452.20ns       389128
ListsDs        4096     100.91ns   19913.02ns     35509.42ns      10.17ns   34570.81ns       778248
ListsDs        8192     103.87ns   39625.52ns     70873.96ns       9.34ns   67139.48ns      1556488
ListsDs       16384     120.37ns   82098.42ns    140047.24ns      10.75ns  146703.38ns      3112968

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
MapsDs            8     171.69ns      31.05ns       266.29ns      43.51ns      77.42ns         1368
MapsDs           16     300.98ns      23.76ns       227.25ns      27.98ns     132.80ns         2696
MapsDs           32     393.56ns      19.75ns       210.74ns      20.38ns     142.17ns         5352
MapsDs           64     393.34ns      18.31ns       214.53ns      24.51ns     288.91ns        11464
MapsDs          128     351.72ns      17.59ns       329.63ns      21.18ns     214.85ns        22872
MapsDs          256     476.08ns      17.26ns       312.12ns      21.33ns     301.09ns        46056
MapsDs          516     349.56ns      16.04ns       338.10ns     137.77ns     166.29ns        93216
MapsDs         1024     379.94ns      16.29ns       355.19ns      47.91ns     197.27ns       183816
MapsDs         2048     382.19ns      65.52ns       412.28ns      35.52ns     324.04ns       367496
MapsDs         4096     423.08ns      22.80ns       562.92ns      42.41ns     185.80ns       736712
MapsDs         8192     429.65ns      16.83ns       473.20ns      34.83ns     205.36ns      1476632
MapsDs        16384     476.16ns      30.42ns       433.51ns      28.81ns     226.87ns      2944584

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
EtsSetDs          8     556.14ns     448.76ns       435.93ns     648.29ns     186.29ns       noSize
EtsSetDs         16     563.26ns     301.91ns       335.00ns     357.41ns     153.47ns       noSize
EtsSetDs         32     267.46ns     191.78ns       329.85ns     271.00ns     171.00ns       noSize
EtsSetDs         64     252.53ns     146.92ns       314.54ns     249.02ns     130.76ns       noSize
EtsSetDs        128     290.61ns     147.13ns       354.75ns     250.12ns     128.83ns       noSize
EtsSetDs        256     257.39ns     134.75ns       307.38ns     244.42ns     132.77ns       noSize
EtsSetDs        516     259.48ns     134.36ns       316.14ns     242.22ns     142.24ns       noSize
EtsSetDs       1024     269.34ns     127.69ns       311.62ns     241.83ns     138.93ns       noSize
EtsSetDs       2048     276.17ns     126.39ns       302.67ns     243.99ns     142.53ns       noSize
EtsSetDs       4096     277.66ns     128.92ns       312.67ns     243.68ns     146.29ns       noSize
EtsSetDs       8192     275.33ns     127.47ns       297.64ns     235.53ns     152.29ns       noSize
EtsSetDs      16384     285.70ns     129.12ns       302.68ns     243.05ns     149.40ns       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
EtsOrdDs          8     718.06ns     574.54ns       390.86ns     482.69ns     186.68ns       noSize
EtsOrdDs         16     293.32ns     312.64ns       351.95ns     299.98ns     164.45ns       noSize
EtsOrdDs         32     312.88ns     211.93ns       424.97ns     227.63ns     164.07ns       noSize
EtsOrdDs         64     312.61ns     217.78ns       350.88ns     209.38ns     165.09ns       noSize
EtsOrdDs        128     326.83ns     182.09ns       361.49ns     193.11ns     179.55ns       noSize
EtsOrdDs        256     331.35ns     190.32ns       384.64ns     204.95ns     167.33ns       noSize
EtsOrdDs        516     352.26ns     193.05ns       387.94ns     195.10ns     173.90ns       noSize
EtsOrdDs       1024     350.40ns     195.44ns       382.27ns     184.06ns     167.20ns       noSize
EtsOrdDs       2048     353.33ns     196.56ns       378.46ns     184.93ns     168.12ns       noSize
EtsOrdDs       4096     347.30ns     203.82ns       393.78ns     187.68ns     169.35ns       noSize
EtsOrdDs       8192     348.57ns     212.98ns       394.20ns     186.06ns     171.60ns       noSize
EtsOrdDs      16384     358.15ns     224.81ns       401.34ns     203.78ns     173.20ns       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
PTermDs           8     266.32ns      72.79ns     notSupport   notSupport   notSupport       noSize
PTermDs          16     258.74ns      56.94ns     notSupport   notSupport   notSupport       noSize
PTermDs          32     359.33ns      51.83ns     notSupport   notSupport   notSupport       noSize
PTermDs          64     273.34ns      77.23ns     notSupport   notSupport   notSupport       noSize
PTermDs         128     287.83ns      69.44ns     notSupport   notSupport   notSupport       noSize
PTermDs         256     273.62ns      58.75ns     notSupport   notSupport   notSupport       noSize
PTermDs         516     289.61ns      55.75ns     notSupport   notSupport   notSupport       noSize
PTermDs        1024     283.18ns      55.41ns     notSupport   notSupport   notSupport       noSize
PTermDs        2048     269.99ns      55.51ns     notSupport   notSupport   notSupport       noSize
PTermDs        4096     269.08ns      55.06ns     notSupport   notSupport   notSupport       noSize
PTermDs        8192     268.18ns      56.30ns     notSupport   notSupport   notSupport       noSize
PTermDs       16384     264.00ns      57.82ns     notSupport   notSupport   notSupport       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
AtomicsDs         8      31.95ns      41.06ns        33.20ns      39.76ns   notSupport       noSize
AtomicsDs        16      26.67ns      34.16ns        26.84ns      33.33ns   notSupport       noSize
AtomicsDs        32      24.80ns      31.54ns        24.54ns      31.23ns   notSupport       noSize
AtomicsDs        64      23.81ns      29.89ns        23.18ns      43.84ns   notSupport       noSize
AtomicsDs       128      23.20ns      29.30ns        22.60ns      29.04ns   notSupport       noSize
AtomicsDs       256      22.94ns      29.95ns        22.19ns      32.86ns   notSupport       noSize
AtomicsDs       516      22.72ns      29.54ns        22.56ns      29.43ns   notSupport       noSize
AtomicsDs      1024      22.35ns      27.75ns        20.96ns      26.96ns   notSupport       noSize
AtomicsDs      2048      22.41ns      27.28ns        21.03ns      26.68ns   notSupport       noSize
AtomicsDs      4096      24.97ns      27.37ns        21.41ns      26.70ns   notSupport       noSize
AtomicsDs      8192      23.66ns      26.30ns        20.87ns      26.42ns   notSupport       noSize
AtomicsDs     16384      22.71ns      26.75ns        21.30ns      26.50ns   notSupport       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
ArrayDs           8      93.06ns      30.13ns       205.66ns      24.43ns   notSupport         1120
ArrayDs          16     296.23ns      35.21ns       193.59ns      33.52ns   notSupport         2280
ArrayDs          32     198.20ns      29.59ns       149.38ns      28.52ns   notSupport         4408
ArrayDs          64     182.89ns      39.47ns       112.30ns      27.10ns   notSupport         8576
ArrayDs         128     198.41ns      50.05ns       180.63ns      46.21ns   notSupport        17104
ArrayDs         256     204.46ns      51.89ns       172.35ns      45.90ns   notSupport        33960
ArrayDs         516     207.41ns      45.85ns       182.28ns      45.70ns   notSupport        68256
ArrayDs        1024     248.67ns      66.31ns       234.48ns      66.66ns   notSupport       135392
ArrayDs        2048     233.34ns      65.68ns       219.70ns      65.36ns   notSupport       270352
ArrayDs        4096     227.97ns      66.59ns       228.82ns      65.69ns   notSupport       540360
ArrayDs        8192     215.55ns      65.68ns       214.66ns      67.05ns   notSupport      1080472
ArrayDs       16384     247.18ns      85.99ns       251.61ns      85.16ns   notSupport      2160800

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
ArrayDs1          8      67.01ns      19.90ns        53.50ns       9.68ns   notSupport         1056
ArrayDs1         16     123.12ns      13.81ns       119.46ns       4.42ns   notSupport         2096
ArrayDs1         32     135.60ns      10.77ns       111.24ns       2.18ns   notSupport         4176
ArrayDs1         64     135.32ns       9.41ns       115.93ns       1.18ns   notSupport         8336
ArrayDs1        128     165.56ns       8.76ns       192.87ns       0.61ns   notSupport        16656
ArrayDs1        256     273.02ns       8.68ns       206.34ns       0.30ns   notSupport        33296
ArrayDs1        516     471.55ns       8.84ns       358.34ns       0.16ns   notSupport        67096
ArrayDs1       1024     738.12ns       8.26ns       903.80ns       0.08ns   notSupport       133136
ArrayDs1       2048    1528.35ns       8.05ns      1851.18ns       0.05ns   notSupport       266256
ArrayDs1       4096    2671.32ns       8.15ns      3147.34ns       0.02ns   notSupport       532496
ArrayDs1       8192    6195.12ns      10.79ns      7604.06ns       0.01ns   notSupport      1064976
ArrayDs1      16384         skip         skip           skip         skip         skip       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
DictDs            8     852.81ns     144.21ns       410.43ns      70.36ns     567.51ns         1832
DictDs           16     697.03ns     142.19ns       425.02ns      37.60ns     309.61ns         3288
DictDs           32     510.16ns     137.64ns       526.98ns      27.81ns     239.61ns         6200
DictDs           64     520.99ns     155.38ns       440.07ns      20.30ns     238.33ns        12024
DictDs          128     751.10ns     182.41ns       569.49ns      17.11ns     273.07ns        23816
DictDs          256     840.65ns     186.88ns       462.72ns      16.15ns     328.73ns        47400
DictDs          516     686.26ns     164.45ns       495.60ns      15.92ns     316.75ns        95296
DictDs         1024     712.80ns     153.79ns       522.81ns      15.20ns     303.18ns       188904
DictDs         2048     711.50ns     157.46ns       490.98ns      15.67ns     296.10ns       377576
DictDs         4096     732.45ns     173.49ns       603.23ns      17.00ns     318.56ns       754920
DictDs         8192     792.71ns     191.22ns       512.56ns      17.47ns     342.32ns      1509608
DictDs        16384     876.25ns     219.62ns       730.63ns      21.09ns     435.42ns      3018984

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
Gb_treesDs        8     296.16ns     243.09ns       366.65ns      31.54ns     244.47ns         1552
Gb_treesDs       16     765.11ns     285.81ns       236.01ns     190.16ns     243.22ns         3072
Gb_treesDs       32     599.99ns     178.20ns       388.75ns     129.33ns     303.73ns         6112
Gb_treesDs       64     829.52ns     194.15ns       330.67ns      67.75ns     321.35ns        12192
Gb_treesDs      128     996.37ns     160.34ns       366.65ns      62.31ns     253.93ns        24352
Gb_treesDs      256     896.02ns     173.52ns       421.27ns      14.74ns     281.09ns        48672
Gb_treesDs      516     900.28ns     201.51ns       336.87ns      84.00ns     333.94ns        98072
Gb_treesDs     1024    1006.44ns     206.19ns       348.64ns      26.72ns     298.02ns       194592
Gb_treesDs     2048    1054.95ns     224.23ns       357.46ns      33.78ns     310.77ns       389152
Gb_treesDs     4096    1173.94ns     233.57ns       366.55ns      21.27ns     335.97ns       778272
Gb_treesDs     8192    1231.74ns     253.28ns       515.64ns      14.83ns     357.72ns      1556512
Gb_treesDs    16384    1320.34ns     271.85ns       402.88ns      99.65ns     470.59ns      3112992

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SetsDs            8     533.71ns     131.20ns     notSupport      61.11ns     427.69ns          728
SetsDs           16     509.56ns     125.77ns     notSupport      36.37ns     307.79ns         1080
SetsDs           32     337.04ns     134.26ns     notSupport      21.94ns     324.17ns         1784
SetsDs           64     303.08ns     172.72ns     notSupport      16.19ns     348.32ns         3192
SetsDs          128     427.33ns     201.58ns     notSupport      13.44ns     480.35ns         6152
SetsDs          256     466.40ns     181.59ns     notSupport      12.53ns     474.89ns        12072
SetsDs          516     476.12ns     135.52ns     notSupport      11.82ns     430.62ns        24088
SetsDs         1024     510.93ns     140.17ns     notSupport      12.02ns     415.17ns        47592
SetsDs         2048     526.86ns     140.68ns     notSupport      11.59ns     483.56ns        94952
SetsDs         4096     610.98ns     146.48ns     notSupport      11.55ns     449.67ns       189672
SetsDs         8192     612.39ns     152.52ns     notSupport      12.02ns     528.45ns       379112
SetsDs        16384     644.74ns     160.43ns     notSupport      12.25ns     610.44ns       757992

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
Gb_setsDs         8     221.76ns     117.06ns     notSupport      35.27ns     290.65ns          512
Gb_setsDs        16     487.61ns     110.47ns     notSupport      21.86ns     234.64ns          992
Gb_setsDs        32     488.39ns     125.93ns     notSupport      18.48ns     277.36ns         1952
Gb_setsDs        64     703.07ns     171.38ns     notSupport      17.24ns     238.10ns         3872
Gb_setsDs       128     880.97ns     155.66ns     notSupport      16.37ns     246.69ns         7712
Gb_setsDs       256     889.17ns     173.21ns     notSupport      16.46ns     270.79ns        15392
Gb_setsDs       516     900.78ns     198.06ns     notSupport      15.40ns     276.39ns        30992
Gb_setsDs      1024    1017.21ns     201.84ns     notSupport      14.09ns     300.26ns        61472
Gb_setsDs      2048    1084.55ns     220.52ns     notSupport      13.90ns     314.25ns       122912
Gb_setsDs      4096    1166.61ns     231.15ns     notSupport      13.84ns     337.84ns       245792
Gb_setsDs      8192    1265.87ns     246.58ns     notSupport      13.71ns     397.11ns       491552
Gb_setsDs     16384    1383.44ns     267.69ns     notSupport      13.49ns     412.40ns       983072

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
OrddictDs         8     150.55ns     117.74ns       538.94ns      36.66ns     127.86ns         1528
OrddictDs        16     216.93ns     156.40ns       457.39ns      25.48ns     240.01ns         3048
OrddictDs        32     339.88ns     284.14ns       722.30ns      18.71ns     392.58ns         6088
OrddictDs        64     556.62ns     479.90ns      1157.47ns      15.44ns     683.83ns        12168
OrddictDs       128     964.64ns     886.80ns      1888.09ns      13.74ns    1229.85ns        24328
OrddictDs       256    1426.80ns    1685.34ns      3540.77ns      12.61ns    2351.11ns        48648
OrddictDs       516    2525.47ns    3372.46ns      7181.50ns      12.53ns    4749.19ns        98048
OrddictDs      1024    4827.93ns    6660.21ns     14262.92ns      12.48ns    9417.98ns       194568
OrddictDs      2048    9699.17ns   13505.77ns     27987.07ns      12.52ns   18620.24ns       389128
OrddictDs      4096   19029.30ns   26853.34ns     56195.41ns      12.75ns   37660.46ns       778248
OrddictDs      8192   38168.74ns   54107.89ns    115824.62ns      12.78ns   77517.15ns      1556488
OrddictDs     16384   77589.02ns  106934.69ns    236854.76ns      12.61ns  159427.63ns      3112968

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
OrdsetsDs         8     105.38ns     107.00ns     notSupport      36.64ns      91.08ns          360
OrdsetsDs        16     113.62ns     156.40ns     notSupport      20.71ns     271.66ns          712
OrdsetsDs        32     173.22ns     296.63ns     notSupport      15.03ns     245.19ns         1416
OrdsetsDs        64     286.53ns     526.02ns     notSupport      11.54ns     459.89ns         2824
OrdsetsDs       128     545.55ns     890.65ns     notSupport       9.50ns     784.49ns         5640
OrdsetsDs       256     901.95ns    1621.12ns     notSupport       8.59ns    1394.46ns        11272
OrdsetsDs       516    1535.25ns    3208.82ns     notSupport       8.11ns    2677.88ns        22712
OrdsetsDs      1024    2825.60ns    6342.30ns     notSupport       8.19ns    5329.68ns        45064
OrdsetsDs      2048    5553.64ns   12724.53ns     notSupport       8.09ns   10851.65ns        90120
OrdsetsDs      4096   11025.72ns   25404.72ns     notSupport       8.07ns   21891.09ns       180232
OrdsetsDs      8192   22317.79ns   51148.03ns     notSupport       8.02ns   44336.62ns       360456
OrdsetsDs     16384   44215.51ns  103279.11ns     notSupport       8.18ns   88753.96ns       720904

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
HashBblDs         8    1097.83ns     137.07ns      1017.67ns       9.81ns   notSupport       noSize
HashBblDs        16     879.00ns     113.38ns       480.04ns       4.84ns   notSupport       noSize
HashBblDs        32     686.17ns     108.77ns       424.99ns       2.43ns   notSupport       noSize
HashBblDs        64     638.10ns     106.83ns       558.58ns       1.25ns   notSupport       noSize
HashBblDs       128     670.94ns     106.70ns       531.51ns       0.63ns   notSupport       noSize
HashBblDs       256     602.83ns     111.88ns       604.02ns       0.32ns   notSupport       noSize
HashBblDs       516     568.08ns     112.23ns       551.63ns       0.18ns   notSupport       noSize
HashBblDs      1024     568.26ns     111.49ns       696.09ns       0.08ns   notSupport       noSize
HashBblDs      2048     647.30ns     116.96ns       650.30ns       0.05ns   notSupport       noSize
HashBblDs      4096     640.40ns     121.31ns       902.30ns       0.03ns   notSupport       noSize
HashBblDs      8192     809.50ns     147.44ns       763.46ns       0.01ns   notSupport       noSize
HashBblDs     16384     793.60ns     161.85ns       985.62ns       0.01ns   notSupport       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
HashBblDs1        8     933.24ns     133.91ns       381.95ns       9.04ns   notSupport       noSize
HashBblDs1       16     535.56ns     123.11ns       301.01ns       4.58ns   notSupport       noSize
HashBblDs1       32     405.54ns     132.96ns       498.00ns       2.17ns   notSupport       noSize
HashBblDs1       64     503.48ns     126.93ns       456.23ns       1.14ns   notSupport       noSize
HashBblDs1      128     543.29ns     118.44ns       513.36ns       0.57ns   notSupport       noSize
HashBblDs1      256     531.78ns     124.71ns       556.40ns       0.30ns   notSupport       noSize
HashBblDs1      516     529.10ns     133.92ns       528.90ns       0.15ns   notSupport       noSize
HashBblDs1     1024     450.63ns     140.61ns       583.73ns       0.09ns   notSupport       noSize
HashBblDs1     2048     498.93ns     165.57ns       583.15ns       0.05ns   notSupport       noSize
HashBblDs1     4096     614.11ns     230.99ns       672.69ns       0.02ns   notSupport       noSize
HashBblDs1     8192     731.03ns     346.35ns      1030.79ns       0.02ns   notSupport       noSize
HashBblDs1    16384     952.66ns     497.58ns      1423.80ns       0.01ns   notSupport       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
KhashDs           8     613.20ns     204.65ns       469.25ns     183.96ns     518.13ns       noSize
KhashDs          16     515.26ns     252.88ns       393.64ns     242.96ns     445.08ns       noSize
KhashDs          32     466.29ns     256.82ns       371.85ns     199.05ns     367.06ns       noSize
KhashDs          64     501.60ns     212.20ns       346.99ns     196.46ns     350.40ns       noSize
KhashDs         128     581.22ns     209.25ns       339.11ns     189.44ns     374.29ns       noSize
KhashDs         256     535.37ns     200.36ns       356.49ns     192.13ns     355.23ns       noSize
KhashDs         516     613.91ns     184.93ns       338.17ns     194.92ns     352.44ns       noSize
KhashDs        1024     609.11ns     192.71ns       350.89ns     189.72ns     343.38ns       noSize
KhashDs        2048     662.61ns     217.55ns       562.00ns     215.78ns     382.08ns       noSize
KhashDs        4096     745.95ns     252.27ns       627.70ns     282.72ns     422.17ns       noSize
KhashDs        8192     855.01ns     304.98ns       631.00ns     362.10ns     564.58ns       noSize
KhashDs       16384     914.30ns     338.90ns       636.28ns     412.85ns     551.83ns       noSize
ok
