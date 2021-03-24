DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
PdDs              8     185.31ns      81.38ns       119.54ns     136.96ns     193.40ns       noSize
PdDs             16     152.27ns      60.92ns       256.55ns     103.79ns      69.33ns       noSize
PdDs             32     252.99ns      61.46ns       236.35ns     104.16ns      73.18ns       noSize
PdDs             64     440.00ns      62.14ns       139.31ns     190.03ns      83.47ns       noSize
PdDs            128     425.26ns      64.70ns       241.84ns     209.70ns      77.35ns       noSize
PdDs            256     451.93ns      64.74ns       139.96ns     183.56ns      81.08ns       noSize
PdDs            516     382.52ns      64.32ns       178.37ns     182.13ns      82.45ns       noSize
PdDs           1024     353.92ns     110.07ns       136.36ns     176.65ns      77.72ns       noSize
PdDs           2048     374.15ns      85.00ns       221.10ns     115.74ns      78.25ns       noSize
PdDs           4096     305.37ns     111.73ns       244.70ns     157.44ns      82.55ns       noSize
PdDs           8192     360.55ns      72.25ns       188.60ns     202.25ns      87.29ns       noSize
PdDs          16384     380.17ns     131.08ns       198.75ns     177.76ns      93.19ns       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
TupleDs           8      66.69ns      16.10ns        53.75ns      15.26ns   notSupport         1056
TupleDs          16     120.74ns      10.15ns       115.96ns       9.87ns   notSupport         2096
TupleDs          32     128.57ns       6.77ns       105.92ns       6.72ns   notSupport         4176
TupleDs          64     137.71ns       5.28ns       119.33ns       5.03ns   notSupport         8336
TupleDs         128     161.14ns       4.38ns       187.22ns       4.32ns   notSupport        16656
TupleDs         256     288.17ns       3.98ns       198.81ns       3.88ns   notSupport        33296
TupleDs         516     475.31ns       3.70ns       419.11ns       3.63ns   notSupport        67096
TupleDs        1024     731.28ns       3.48ns       871.09ns       3.46ns   notSupport       133136
TupleDs        2048    1533.95ns       3.35ns      1828.76ns       3.37ns   notSupport       266256
TupleDs        4096    2733.13ns       3.39ns      3209.87ns       3.43ns   notSupport       532496
TupleDs        8192    6097.60ns       3.36ns      7355.53ns       3.42ns   notSupport      1064976
TupleDs       16384   10295.45ns       3.39ns     11962.81ns       3.37ns   notSupport      2129936

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
ListsDs           8      53.11ns     116.13ns       325.56ns      12.54ns     294.93ns         1528
ListsDs          16      44.11ns     127.72ns       391.54ns       7.33ns     238.96ns         3048
ListsDs          32      77.52ns     252.78ns       476.18ns       5.51ns     444.34ns         6088
ListsDs          64     114.42ns     370.76ns       772.02ns       4.41ns     624.63ns        12168
ListsDs         128     200.97ns     691.01ns      1343.26ns       4.50ns    1126.81ns        24328
ListsDs         256     169.27ns    1300.45ns      2318.20ns       4.86ns    2099.85ns        48648
ListsDs         516     164.67ns    2491.28ns      4302.55ns       5.29ns    4364.19ns        98048
ListsDs        1024     110.81ns    4922.59ns      8768.78ns       5.38ns    8439.21ns       194568
ListsDs        2048     137.75ns    9854.12ns     17232.39ns       5.97ns   17428.40ns       389128
ListsDs        4096     118.65ns   19880.46ns     35017.51ns      11.65ns   34639.02ns       778248
ListsDs        8192     120.81ns   39731.49ns     70116.04ns       9.51ns   67302.19ns      1556488
ListsDs       16384     129.54ns   81948.58ns    139715.24ns      11.28ns  149049.44ns      3112968

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
MapsDs            8     176.13ns      31.48ns       294.48ns      48.54ns      80.96ns         1368
MapsDs           16     311.70ns      23.85ns       241.41ns      29.63ns     149.92ns         2696
MapsDs           32     396.99ns      20.64ns       212.22ns      21.11ns     144.25ns         5352
MapsDs           64     417.99ns      18.77ns       340.06ns      26.79ns     234.67ns        11480
MapsDs          128     443.13ns      18.04ns       369.59ns      22.25ns     246.41ns        22904
MapsDs          256     370.66ns      17.26ns       331.83ns      88.95ns     236.91ns        46072
MapsDs          516     382.03ns      40.98ns       400.35ns      71.22ns     194.39ns        93008
MapsDs         1024     411.96ns      16.28ns       360.39ns      43.08ns     196.81ns       184040
MapsDs         2048     374.38ns      30.29ns       407.12ns      32.93ns     330.12ns       367224
MapsDs         4096     429.01ns      15.99ns       422.94ns      35.45ns     209.50ns       736792
MapsDs         8192     445.63ns      24.65ns       591.63ns      22.55ns     207.55ns      1476776
MapsDs        16384     467.64ns      24.05ns       595.20ns      26.48ns     264.62ns      2944216

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
EtsSetDs          8     330.88ns     383.65ns       401.41ns     596.08ns     202.01ns       noSize
EtsSetDs         16     262.26ns     302.22ns       353.52ns     345.39ns     167.44ns       noSize
EtsSetDs         32     271.92ns     186.16ns       358.08ns     279.18ns     153.58ns       noSize
EtsSetDs         64     263.68ns     150.10ns       343.57ns     259.08ns     144.30ns       noSize
EtsSetDs        128     288.63ns     168.86ns       368.09ns     255.20ns     144.12ns       noSize
EtsSetDs        256     302.87ns     146.16ns       356.34ns     255.61ns     128.82ns       noSize
EtsSetDs        516     282.86ns     144.27ns       341.40ns     258.64ns     123.20ns       noSize
EtsSetDs       1024     302.16ns     135.71ns       336.15ns     261.23ns     127.46ns       noSize
EtsSetDs       2048     308.25ns     135.13ns       325.52ns     250.28ns     131.69ns       noSize
EtsSetDs       4096     326.46ns     130.49ns       311.32ns     244.75ns     134.05ns       noSize
EtsSetDs       8192     305.36ns     129.20ns       330.04ns     238.52ns     141.91ns       noSize
EtsSetDs      16384     299.77ns     135.95ns       312.88ns     241.09ns     159.20ns       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
EtsOrdDs          8     400.64ns     565.66ns       417.85ns     853.08ns     194.84ns       noSize
EtsOrdDs         16     308.18ns     311.73ns       376.61ns     290.48ns     176.64ns       noSize
EtsOrdDs         32     330.86ns     217.31ns       384.36ns     223.17ns     175.53ns       noSize
EtsOrdDs         64     336.25ns     196.59ns       373.10ns     213.04ns     180.52ns       noSize
EtsOrdDs        128     335.46ns     201.52ns       374.91ns     200.13ns     183.02ns       noSize
EtsOrdDs        256     364.59ns     195.44ns       396.42ns     213.45ns     167.15ns       noSize
EtsOrdDs        516     365.25ns     202.81ns       379.42ns     203.48ns     153.81ns       noSize
EtsOrdDs       1024     381.69ns     201.55ns       423.33ns     207.02ns     161.40ns       noSize
EtsOrdDs       2048     382.07ns     202.62ns       407.94ns     194.37ns     165.40ns       noSize
EtsOrdDs       4096     382.95ns     209.63ns       403.91ns     191.16ns     157.49ns       noSize
EtsOrdDs       8192     367.69ns     215.85ns       416.79ns     186.72ns     163.96ns       noSize
EtsOrdDs      16384     375.41ns     227.63ns       417.54ns     203.17ns     179.51ns       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
PTermDs           8     283.34ns      66.28ns     notSupport   notSupport   notSupport       noSize
PTermDs          16     252.74ns      61.85ns     notSupport   notSupport   notSupport       noSize
PTermDs          32     316.34ns      58.77ns     notSupport   notSupport   notSupport       noSize
PTermDs          64     290.42ns      74.92ns     notSupport   notSupport   notSupport       noSize
PTermDs         128     285.73ns      73.63ns     notSupport   notSupport   notSupport       noSize
PTermDs         256     330.23ns      66.81ns     notSupport   notSupport   notSupport       noSize
PTermDs         516     313.71ns      64.88ns     notSupport   notSupport   notSupport       noSize
PTermDs        1024     299.01ns      59.63ns     notSupport   notSupport   notSupport       noSize
PTermDs        2048     288.56ns      58.12ns     notSupport   notSupport   notSupport       noSize
PTermDs        4096     280.00ns      58.58ns     notSupport   notSupport   notSupport       noSize
PTermDs        8192     282.57ns      60.25ns     notSupport   notSupport   notSupport       noSize
PTermDs       16384     274.45ns      61.44ns     notSupport   notSupport   notSupport       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
AtomicsDs         8      36.64ns      41.71ns        32.92ns      41.42ns   notSupport       noSize
AtomicsDs        16      28.42ns      34.26ns        26.19ns      33.57ns   notSupport       noSize
AtomicsDs        32      25.31ns      31.19ns        24.18ns      30.86ns   notSupport       noSize
AtomicsDs        64      23.61ns      29.62ns        22.98ns      29.71ns   notSupport       noSize
AtomicsDs       128      22.91ns      28.99ns        22.62ns      28.84ns   notSupport       noSize
AtomicsDs       256      22.52ns      28.53ns        22.30ns      28.47ns   notSupport       noSize
AtomicsDs       516      22.40ns      28.27ns        22.24ns      29.44ns   notSupport       noSize
AtomicsDs      1024      22.29ns      28.62ns        22.18ns      28.93ns   notSupport       noSize
AtomicsDs      2048      26.38ns      31.94ns        21.86ns      28.95ns   notSupport       noSize
AtomicsDs      4096      26.56ns      27.70ns        21.90ns      27.68ns   notSupport       noSize
AtomicsDs      8192      24.14ns      28.49ns        22.93ns      28.22ns   notSupport       noSize
AtomicsDs     16384      23.84ns      27.33ns        22.09ns      27.54ns   notSupport       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
ArrayDs           8      92.71ns      29.06ns       222.29ns      24.52ns   notSupport         1120
ArrayDs          16     271.49ns      36.74ns       163.76ns      34.89ns   notSupport         2280
ArrayDs          32     214.77ns      32.16ns       166.58ns      30.71ns   notSupport         4408
ArrayDs          64     183.93ns      29.69ns       114.14ns      28.99ns   notSupport         8576
ArrayDs         128     206.24ns      49.65ns       197.87ns      49.25ns   notSupport        17104
ArrayDs         256     212.78ns      48.90ns       177.89ns      48.72ns   notSupport        33960
ArrayDs         516     210.17ns      49.10ns       196.03ns      48.42ns   notSupport        68256
ArrayDs        1024     273.26ns      69.62ns       247.14ns      67.74ns   notSupport       135392
ArrayDs        2048     244.10ns      66.96ns       232.11ns      67.45ns   notSupport       270352
ArrayDs        4096     237.90ns      67.28ns       235.61ns      67.28ns   notSupport       540360
ArrayDs        8192     227.42ns      66.62ns       223.39ns      67.63ns   notSupport      1080472
ArrayDs       16384     254.56ns      88.45ns       260.48ns      87.92ns   notSupport      2160800

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
ArrayDs1          8      61.04ns      19.66ns        54.20ns       9.20ns   notSupport         1056
ArrayDs1         16     137.59ns      14.49ns       122.11ns       4.51ns   notSupport         2096
ArrayDs1         32     130.23ns      11.38ns       106.25ns       2.27ns   notSupport         4176
ArrayDs1         64     145.38ns       9.95ns       128.41ns       1.14ns   notSupport         8336
ArrayDs1        128     168.40ns       9.21ns       195.26ns       0.63ns   notSupport        16656
ArrayDs1        256     283.79ns       8.80ns       200.95ns       0.31ns   notSupport        33296
ArrayDs1        516     501.55ns       8.92ns       409.85ns       0.15ns   notSupport        67096
ArrayDs1       1024     764.39ns       8.51ns       869.52ns       0.08ns   notSupport       133136
ArrayDs1       2048    1545.75ns       8.20ns      1823.33ns       0.04ns   notSupport       266256
ArrayDs1       4096    2664.91ns       8.18ns      3116.78ns       0.02ns   notSupport       532496
ArrayDs1       8192    6252.70ns       8.18ns      7444.42ns       0.01ns   notSupport      1064976
ArrayDs1      16384         skip         skip           skip         skip         skip       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
DictDs            8     671.17ns     318.71ns       589.74ns      66.49ns     382.66ns         1832
DictDs           16     457.04ns     140.72ns       404.77ns      38.42ns     274.63ns         3288
DictDs           32     498.25ns     150.78ns       546.61ns      26.00ns     256.89ns         6200
DictDs           64     624.38ns     169.34ns       595.98ns      18.65ns     242.59ns        12024
DictDs          128     657.29ns     192.42ns       597.37ns      17.91ns     285.95ns        23816
DictDs          256     882.83ns     189.47ns       463.11ns      16.52ns     334.80ns        47400
DictDs          516     749.56ns     168.47ns       581.04ns      17.62ns     348.58ns        95296
DictDs         1024     741.63ns     158.04ns       589.90ns      16.26ns     309.94ns       188904
DictDs         2048     747.55ns     162.25ns       499.91ns      16.41ns     304.81ns       377576
DictDs         4096     780.80ns     174.14ns       698.89ns      16.97ns     420.98ns       754920
DictDs         8192     831.21ns     194.22ns       524.66ns      17.99ns     349.57ns      1509608
DictDs        16384     889.71ns     219.03ns       745.57ns      22.25ns     440.91ns      3018984

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
Gb_treesDs        8     237.88ns     423.43ns       528.56ns      30.25ns     252.06ns         1552
Gb_treesDs       16     535.14ns     181.44ns       231.87ns      98.34ns     234.64ns         3072
Gb_treesDs       32     548.58ns     183.41ns       376.98ns     138.12ns     307.59ns         6112
Gb_treesDs       64     918.43ns     198.71ns       354.28ns      70.16ns     345.61ns        12192
Gb_treesDs      128     964.13ns     180.84ns       415.48ns      66.72ns     270.25ns        24352
Gb_treesDs      256     975.37ns     191.31ns       469.22ns      15.92ns     293.73ns        48672
Gb_treesDs      516     978.90ns     208.34ns       345.53ns      86.54ns     334.22ns        98072
Gb_treesDs     1024    1054.82ns     212.68ns       357.41ns      27.56ns     302.18ns       194592
Gb_treesDs     2048    1115.46ns     225.46ns       364.74ns      35.37ns     312.27ns       389152
Gb_treesDs     4096    1190.16ns     248.13ns       370.27ns      22.10ns     333.42ns       778272
Gb_treesDs     8192    1282.37ns     254.13ns       515.36ns      15.36ns     357.25ns      1556512
Gb_treesDs    16384    1352.07ns     266.66ns       400.60ns      99.40ns     467.90ns      3112992

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SetsDs            8     493.09ns     135.86ns     notSupport      61.80ns     531.09ns          728
SetsDs           16     386.40ns     129.69ns     notSupport      37.27ns     441.52ns         1080
SetsDs           32     328.52ns     134.44ns     notSupport      22.70ns     358.70ns         1784
SetsDs           64     311.66ns     144.50ns     notSupport      17.24ns     374.51ns         3192
SetsDs          128     467.97ns     214.85ns     notSupport      14.43ns     498.49ns         6152
SetsDs          256     549.15ns     177.57ns     notSupport      13.28ns     506.58ns        12072
SetsDs          516     550.30ns     140.16ns     notSupport      12.05ns     460.02ns        24088
SetsDs         1024     545.29ns     146.06ns     notSupport      12.60ns     410.45ns        47592
SetsDs         2048     551.88ns     142.54ns     notSupport      12.20ns     483.38ns        94952
SetsDs         4096     627.87ns     147.94ns     notSupport      12.01ns     458.81ns       189672
SetsDs         8192     611.57ns     162.44ns     notSupport      12.57ns     536.93ns       379112
SetsDs        16384     659.38ns     162.91ns     notSupport      12.51ns     616.13ns       757992

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
Gb_setsDs         8     230.43ns     110.85ns     notSupport      36.85ns     493.56ns          512
Gb_setsDs        16     567.23ns     113.71ns     notSupport      24.41ns     425.66ns          992
Gb_setsDs        32     534.28ns     132.23ns     notSupport      20.69ns     280.54ns         1952
Gb_setsDs        64     811.66ns     181.58ns     notSupport      19.15ns     249.17ns         3872
Gb_setsDs       128    1041.33ns     159.13ns     notSupport      17.95ns     276.67ns         7712
Gb_setsDs       256     993.71ns     182.22ns     notSupport      19.56ns     284.33ns        15392
Gb_setsDs       516     994.54ns     199.90ns     notSupport      15.93ns     277.85ns        30992
Gb_setsDs      1024    1063.50ns     210.20ns     notSupport      14.87ns     304.13ns        61472
Gb_setsDs      2048    1133.89ns     224.37ns     notSupport      14.51ns     311.24ns       122912
Gb_setsDs      4096    1183.10ns     237.45ns     notSupport      14.51ns     325.49ns       245792
Gb_setsDs      8192    1300.18ns     245.85ns     notSupport      13.97ns     393.54ns       491552
Gb_setsDs     16384    1431.06ns     261.50ns     notSupport      13.65ns     400.50ns       983072

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
OrddictDs         8     161.75ns     120.69ns      1142.90ns      37.94ns     136.89ns         1528
OrddictDs        16     242.53ns     162.51ns       484.97ns      23.20ns     301.90ns         3048
OrddictDs        32     380.49ns     323.40ns       745.07ns      18.23ns     439.78ns         6088
OrddictDs        64     672.98ns     631.03ns      1276.96ns      16.20ns     715.34ns        12168
OrddictDs       128    1008.74ns     938.30ns      2024.83ns      14.11ns    1263.08ns        24328
OrddictDs       256    1528.11ns    1715.75ns      3650.16ns      13.17ns    2376.19ns        48648
OrddictDs       516    2555.57ns    3312.99ns      7216.16ns      12.54ns    4754.47ns        98048
OrddictDs      1024    4846.28ns    6812.81ns     14350.85ns      12.72ns    9353.44ns       194568
OrddictDs      2048    9775.25ns   13535.06ns     28080.54ns      12.71ns   18614.06ns       389128
OrddictDs      4096   19152.27ns   27332.85ns     55943.65ns      12.77ns   37593.43ns       778248
OrddictDs      8192   38116.21ns   55959.34ns    114791.66ns      13.15ns   77618.22ns      1556488
OrddictDs     16384   77389.72ns  107703.34ns    234827.12ns      12.71ns  159696.28ns      3112968

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
OrdsetsDs         8     125.16ns     115.53ns     notSupport      36.31ns     100.24ns          360
OrdsetsDs        16     114.41ns     154.26ns     notSupport      21.47ns     201.46ns          712
OrdsetsDs        32     176.63ns     293.03ns     notSupport      14.73ns     243.38ns         1416
OrdsetsDs        64     283.62ns     566.40ns     notSupport      11.30ns     564.44ns         2824
OrdsetsDs       128     622.77ns     903.71ns     notSupport       9.82ns     798.25ns         5640
OrdsetsDs       256    1008.41ns    1664.68ns     notSupport       9.90ns    1450.61ns        11272
OrdsetsDs       516    1550.11ns    3266.23ns     notSupport       8.18ns    2710.09ns        22712
OrdsetsDs      1024    2812.89ns    6327.77ns     notSupport       8.19ns    5408.53ns        45064
OrdsetsDs      2048    5566.02ns   12479.70ns     notSupport       7.87ns   10847.86ns        90120
OrdsetsDs      4096   10916.47ns   25211.07ns     notSupport       7.94ns   21810.09ns       180232
OrdsetsDs      8192   21919.60ns   50770.21ns     notSupport       7.97ns   44240.02ns       360456
OrdsetsDs     16384   43336.71ns  101989.89ns     notSupport       7.94ns   88525.71ns       720904

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
HashBblDs         8    1031.29ns     134.66ns      1338.44ns       9.84ns   notSupport       noSize
HashBblDs        16     947.24ns     118.39ns       518.69ns       4.89ns   notSupport       noSize
HashBblDs        32     774.94ns     117.67ns       455.05ns       2.48ns   notSupport       noSize
HashBblDs        64     780.93ns     111.85ns       652.38ns       1.23ns   notSupport       noSize
HashBblDs       128     714.81ns     115.90ns       575.48ns       0.71ns   notSupport       noSize
HashBblDs       256     657.28ns     123.36ns       665.51ns       0.35ns   notSupport       noSize
HashBblDs       516     638.62ns     116.00ns       567.15ns       0.19ns   notSupport       noSize
HashBblDs      1024     585.13ns     117.28ns       667.14ns       0.09ns   notSupport       noSize
HashBblDs      2048     635.84ns     128.29ns       657.16ns       0.05ns   notSupport       noSize
HashBblDs      4096     640.62ns     121.70ns       892.67ns       0.04ns   notSupport       noSize
HashBblDs      8192     786.39ns     149.97ns       765.46ns       0.01ns   notSupport       noSize
HashBblDs     16384     782.55ns     164.11ns       994.29ns       0.01ns   notSupport       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
HashBblDs1        8     785.20ns     153.47ns       664.15ns       9.84ns   notSupport       noSize
HashBblDs1       16     519.54ns     127.90ns       320.08ns       5.00ns   notSupport       noSize
HashBblDs1       32     432.66ns     121.48ns       556.36ns       2.36ns   notSupport       noSize
HashBblDs1       64     543.93ns     125.39ns       504.23ns       1.35ns   notSupport       noSize
HashBblDs1      128     591.54ns     126.36ns       561.74ns       0.73ns   notSupport       noSize
HashBblDs1      256     596.05ns     132.42ns       595.33ns       0.36ns   notSupport       noSize
HashBblDs1      516     621.06ns     139.64ns       551.52ns       0.17ns   notSupport       noSize
HashBblDs1     1024     511.04ns     145.11ns       615.90ns       0.09ns   notSupport       noSize
HashBblDs1     2048     530.40ns     169.94ns       590.10ns       0.05ns   notSupport       noSize
HashBblDs1     4096     620.36ns     230.26ns       690.76ns       0.02ns   notSupport       noSize
HashBblDs1     8192     753.73ns     348.14ns      1041.93ns       0.02ns   notSupport       noSize
HashBblDs1    16384     989.19ns     495.33ns      1442.56ns       0.01ns   notSupport       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
KhashDs           8    1003.35ns     268.00ns       843.34ns     150.82ns     825.05ns       noSize
KhashDs          16     571.38ns     273.56ns       429.93ns     247.74ns     506.58ns       noSize
KhashDs          32     479.01ns     274.64ns       349.83ns     218.50ns     382.59ns       noSize
KhashDs          64     530.47ns     226.32ns       358.86ns     220.04ns     421.88ns       noSize
KhashDs         128     510.07ns     216.59ns       376.79ns     205.72ns     449.60ns       noSize
KhashDs         256     548.75ns     207.33ns       389.98ns     212.38ns     390.41ns       noSize
KhashDs         516     653.27ns     199.99ns       358.02ns     254.00ns     374.40ns       noSize
KhashDs        1024     641.85ns     209.27ns       625.68ns     233.32ns     358.42ns       noSize
KhashDs        2048     758.69ns     219.27ns       501.92ns     250.43ns     396.34ns       noSize
KhashDs        4096     728.83ns     263.04ns       643.43ns     296.28ns     443.68ns       noSize
KhashDs        8192     801.99ns     303.25ns       586.72ns     385.66ns     498.35ns       noSize
KhashDs       16384     914.86ns     335.60ns       643.50ns     390.81ns     546.48ns       noSize
