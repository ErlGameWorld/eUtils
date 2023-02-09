
DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SPdDs             8      86.51ns      18.86ns        26.44ns      59.56ns      35.54ns       noSize
SPdDs            16      69.81ns      11.19ns        18.24ns      43.41ns      23.96ns       noSize
SPdDs            32      50.29ns       8.74ns        15.37ns      44.56ns      50.53ns       noSize
SPdDs            64      74.73ns       6.69ns        13.48ns      39.51ns      31.86ns       noSize
SPdDs           128      77.42ns       5.56ns        12.73ns      63.36ns      17.51ns       noSize
SPdDs           256      72.50ns       4.91ns        12.32ns      37.80ns      20.58ns       noSize
SPdDs           516      64.63ns       4.44ns        11.49ns      35.57ns      23.27ns       noSize
SPdDs          1024      70.92ns       4.36ns        12.63ns      36.41ns      16.83ns       noSize
SPdDs          2048      56.15ns       4.27ns        11.88ns      54.39ns      12.79ns       noSize
SPdDs          4096      55.03ns       4.46ns        11.43ns      42.36ns      12.63ns       noSize
SPdDs          8192      59.05ns       4.35ns        11.64ns      52.56ns      12.27ns       noSize
SPdDs         16384      64.59ns       4.23ns        11.21ns      57.14ns      12.15ns       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
STupleDs          8      42.94ns      14.90ns        32.17ns      14.54ns   notSupport           80
STupleDs         16      53.74ns       9.32ns       121.70ns       8.66ns   notSupport          144
STupleDs         32     137.40ns       6.48ns        46.52ns       6.15ns   notSupport          272
STupleDs         64     104.42ns       4.59ns        88.66ns       4.37ns   notSupport          528
STupleDs        128     138.74ns       3.62ns       114.25ns       3.52ns   notSupport         1040
STupleDs        256     194.62ns       3.18ns       185.48ns       3.09ns   notSupport         2064
STupleDs        516     395.85ns       6.08ns       361.71ns       2.78ns   notSupport         4144
STupleDs       1024     540.89ns       2.61ns       522.04ns       2.58ns   notSupport         8208
STupleDs       2048    1451.16ns       2.67ns      1427.96ns       2.50ns   notSupport        16400
STupleDs       4096    2812.99ns       2.58ns      2836.46ns       2.66ns   notSupport        32784
STupleDs       8192    9350.71ns       6.01ns      9307.78ns       2.65ns   notSupport        65552
STupleDs      16384   23278.40ns       6.83ns     23317.29ns       2.75ns   notSupport       131088

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SListsDs          8      18.61ns      32.81ns        54.65ns      11.75ns      53.45ns          328
SListsDs         16      13.00ns      29.48ns       142.43ns       7.59ns     145.33ns          648
SListsDs         32      10.10ns      41.79ns       251.61ns       5.16ns     172.64ns         1288
SListsDs         64      25.78ns      72.18ns       385.38ns       4.24ns     320.59ns         2568
SListsDs        128      43.98ns     130.28ns       565.29ns       3.79ns     501.04ns         5128
SListsDs        256      71.55ns     244.26ns       846.28ns       3.50ns     818.01ns        10248
SListsDs        516      76.20ns     468.68ns      1728.37ns       3.21ns    1566.78ns        20648
SListsDs       1024      47.94ns     937.44ns      3206.46ns       3.78ns    3400.29ns        40968
SListsDs       2048      40.29ns    2061.60ns      6738.85ns       3.86ns    7147.46ns        81928
SListsDs       4096      44.20ns    4363.06ns     13933.96ns       4.10ns   13966.08ns       163848
SListsDs       8192      47.63ns    8828.03ns     29353.59ns       4.45ns   28919.06ns       327688
SListsDs      16384      48.27ns   17780.47ns     57518.24ns       4.85ns   61110.71ns       655368

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SMapsDs           8      60.66ns      21.52ns        55.13ns      61.09ns     273.95ns          168
SMapsDs          16     194.76ns      15.38ns       129.95ns      25.01ns      82.11ns          296
SMapsDs          32     226.49ns      18.30ns        64.71ns      64.88ns      39.84ns          552
SMapsDs          64     204.89ns      15.70ns       198.13ns      24.41ns      87.52ns         1976
SMapsDs         128     197.92ns      16.31ns       192.83ns      19.40ns     122.81ns         3736
SMapsDs         256     198.65ns      16.98ns       156.90ns      21.76ns     134.62ns         7528
SMapsDs         516     219.30ns      16.62ns       233.05ns      49.53ns     127.86ns        15560
SMapsDs        1024     227.51ns      16.28ns       294.35ns      37.97ns     151.35ns        30392
SMapsDs        2048     250.04ns      17.89ns       270.32ns      27.60ns     161.73ns        59896
SMapsDs        4096     284.02ns      18.94ns       316.46ns      35.11ns     151.05ns       122648
SMapsDs        8192     327.13ns      20.82ns       293.24ns      31.71ns     170.86ns       248312
SMapsDs       16384     322.60ns      24.44ns       378.97ns      35.31ns     178.66ns       487576

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SQueueDs          8      28.64ns      59.81ns     notSupport   notSupport   notSupport          160
SQueueDs         16      22.47ns     128.53ns     notSupport   notSupport   notSupport          288
SQueueDs         32      18.71ns      98.71ns     notSupport   notSupport   notSupport          544
SQueueDs         64      28.36ns      76.33ns     notSupport   notSupport   notSupport         1056
SQueueDs        128      33.32ns      69.87ns     notSupport   notSupport   notSupport         2080
SQueueDs        256      39.19ns      51.75ns     notSupport   notSupport   notSupport         4128
SQueueDs        516      31.33ns      59.40ns     notSupport   notSupport   notSupport         8288
SQueueDs       1024      36.53ns      57.25ns     notSupport   notSupport   notSupport        16416
SQueueDs       2048      37.21ns      55.81ns     notSupport   notSupport   notSupport        32800
SQueueDs       4096      28.96ns      67.33ns     notSupport   notSupport   notSupport        65568
SQueueDs       8192      31.21ns      55.23ns     notSupport   notSupport   notSupport       131104
SQueueDs      16384      33.01ns      62.98ns     notSupport   notSupport   notSupport       262176

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SArrayDs          8      71.44ns      26.55ns       189.86ns      26.65ns   notSupport          144
SArrayDs         16     259.01ns      37.52ns       137.28ns      33.84ns   notSupport          328
SArrayDs         32     161.64ns      30.41ns       107.53ns      28.79ns   notSupport          504
SArrayDs         64     142.05ns      27.49ns        85.08ns      26.76ns   notSupport          768
SArrayDs        128     147.79ns      46.30ns       146.73ns      45.90ns   notSupport         1488
SArrayDs        256     149.37ns      47.61ns       146.53ns      45.54ns   notSupport         2728
SArrayDs        516     144.10ns      45.87ns       138.53ns      45.64ns   notSupport         5304
SArrayDs       1024     178.21ns      66.69ns       180.36ns      66.15ns   notSupport        10464
SArrayDs       2048     183.07ns      69.65ns       173.73ns      66.35ns   notSupport        20496
SArrayDs       4096     174.58ns      65.07ns       169.35ns      64.63ns   notSupport        40648
SArrayDs       8192     169.82ns      64.54ns       153.60ns      64.55ns   notSupport        81048
SArrayDs      16384     196.62ns      84.98ns       197.41ns      84.87ns   notSupport       161952

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SEtsSetDs         8     233.04ns     133.16ns       122.67ns     477.19ns     175.43ns        11032
SEtsSetDs        16     189.92ns     110.16ns       104.76ns     386.84ns     150.10ns        11032
SEtsSetDs        32     176.63ns     129.51ns       145.53ns     282.44ns     139.43ns        11032
SEtsSetDs        64     170.38ns     146.51ns       115.66ns     232.46ns     138.19ns        11032
SEtsSetDs       128     178.16ns     120.87ns        97.23ns     228.85ns     138.81ns        11032
SEtsSetDs       256     176.37ns     121.12ns        98.47ns     207.56ns     142.92ns        11032
SEtsSetDs       516     203.20ns     114.89ns       102.82ns     199.80ns     136.26ns        29512
SEtsSetDs      1024     209.44ns     110.92ns        98.73ns     204.07ns     148.49ns        29512
SEtsSetDs      2048     222.79ns     108.91ns        98.36ns     198.87ns     147.45ns        29512
SEtsSetDs      4096     222.34ns     112.90ns       100.86ns     203.14ns     163.63ns        29512
SEtsSetDs      8192     226.76ns     114.81ns       103.24ns     200.76ns     173.00ns        29512
SEtsSetDs     16384     228.31ns     116.59ns       105.82ns     202.75ns     175.99ns        29512

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SEtsOrdDs         8     251.38ns     139.50ns       125.49ns     554.77ns     208.78ns         1336
SEtsOrdDs        16     231.68ns     120.76ns       116.22ns     363.75ns     180.16ns         1336
SEtsOrdDs        32     231.46ns     149.23ns       165.58ns     309.28ns     170.89ns         1336
SEtsOrdDs        64     226.25ns     174.38ns       134.33ns     284.63ns     170.58ns         1336
SEtsOrdDs       128     220.06ns     139.25ns       112.69ns     268.29ns     161.38ns         1336
SEtsOrdDs       256     221.00ns     143.07ns       121.63ns     270.89ns     167.44ns         1336
SEtsOrdDs       516     218.70ns     139.50ns       122.19ns     270.09ns     170.54ns         1336
SEtsOrdDs      1024     219.21ns     133.65ns       119.87ns     276.56ns     171.47ns         1336
SEtsOrdDs      2048     233.98ns     133.00ns       122.30ns     279.32ns     171.08ns         1336
SEtsOrdDs      4096     230.63ns     142.56ns       127.79ns     282.42ns     182.38ns         1336
SEtsOrdDs      8192     226.26ns     138.49ns       128.06ns     279.88ns     173.46ns         1336
SEtsOrdDs     16384     221.42ns     137.32ns       126.71ns     278.06ns     171.07ns         1336

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SeLfqDs           8     422.93ns     305.05ns     notSupport   notSupport   notSupport       noSize
SeLfqDs          16     275.66ns     291.18ns     notSupport   notSupport   notSupport       noSize
SeLfqDs          32     229.98ns     255.98ns     notSupport   notSupport   notSupport       noSize
SeLfqDs          64     207.90ns     235.72ns     notSupport   notSupport   notSupport       noSize
SeLfqDs         128     198.61ns     223.40ns     notSupport   notSupport   notSupport       noSize
SeLfqDs         256     215.64ns     224.64ns     notSupport   notSupport   notSupport       noSize
SeLfqDs         516     214.80ns     214.73ns     notSupport   notSupport   notSupport       noSize
SeLfqDs        1024     206.98ns     208.69ns     notSupport   notSupport   notSupport       noSize
SeLfqDs        2048     194.11ns     207.96ns     notSupport   notSupport   notSupport       noSize
SeLfqDs        4096     190.50ns     203.59ns     notSupport   notSupport   notSupport       noSize
SeLfqDs        8192     186.95ns     201.44ns     notSupport   notSupport   notSupport       noSize
SeLfqDs       16384     186.69ns     201.00ns     notSupport   notSupport   notSupport       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SDictDs           8     978.71ns      87.46ns       359.66ns      60.20ns     244.14ns          632
SDictDs          16     327.80ns     105.48ns       256.15ns      31.49ns     211.40ns          888
SDictDs          32     308.79ns      94.34ns       207.63ns      20.40ns     172.30ns         1400
SDictDs          64     294.52ns      76.67ns       241.04ns      14.96ns     207.28ns         2424
SDictDs         128     339.93ns      90.56ns       278.75ns      13.40ns     183.42ns         4616
SDictDs         256     384.54ns      91.33ns       227.43ns      12.59ns     222.39ns         9000
SDictDs         516     408.04ns      84.11ns       261.15ns      12.83ns     239.51ns        17896
SDictDs        1024     427.21ns      87.57ns       274.22ns      11.84ns     224.81ns        35304
SDictDs        2048     433.82ns      90.48ns       307.94ns      11.84ns     225.21ns        70376
SDictDs        4096     483.01ns     112.11ns       318.66ns      12.63ns     247.07ns       140520
SDictDs        8192     506.73ns     109.65ns       348.95ns      13.77ns     272.48ns       280808
SDictDs       16384     599.55ns     110.04ns       491.11ns      14.14ns     311.55ns       561384

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SGb_treesD        8     223.43ns      32.70ns       139.82ns      22.70ns     124.51ns          352
SGb_treesD       16     254.72ns      28.94ns       110.21ns      47.19ns      55.61ns          672
SGb_treesD       32     400.40ns      44.83ns       118.05ns      15.06ns      66.61ns         1312
SGb_treesD       64     422.39ns      48.37ns       118.39ns      28.40ns      54.68ns         2592
SGb_treesD      128     507.38ns      55.24ns       132.18ns      13.37ns      57.21ns         5152
SGb_treesD      256     601.78ns      65.89ns       123.62ns      12.77ns      60.04ns        10272
SGb_treesD      516     580.29ns      56.55ns       125.04ns      42.31ns      82.08ns        20672
SGb_treesD     1024     624.39ns      61.56ns       156.74ns      23.17ns      64.92ns        40992
SGb_treesD     2048     705.86ns      60.50ns       141.91ns      28.56ns      70.95ns        81952
SGb_treesD     4096     772.28ns      64.02ns       146.02ns      46.62ns      94.86ns       163872
SGb_treesD     8192     817.74ns      76.31ns       172.65ns      34.73ns      75.77ns       327712
SGb_treesD    16384     907.75ns      73.28ns       150.90ns      50.16ns     140.43ns       655392

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SSetsDs           8     582.59ns      85.81ns     notSupport      41.64ns     378.35ns          504
SSetsDs          16     314.44ns      77.16ns     notSupport      22.51ns     303.68ns          632
SSetsDs          32     248.24ns      95.75ns     notSupport      13.41ns     206.93ns          888
SSetsDs          64     243.00ns      74.32ns     notSupport       8.80ns     189.82ns         1400
SSetsDs         128     232.48ns      78.87ns     notSupport       7.41ns     223.74ns         2568
SSetsDs         256     282.71ns      75.86ns     notSupport       7.05ns     262.83ns         4904
SSetsDs         516     300.82ns      72.18ns     notSupport       6.95ns     268.78ns         9640
SSetsDs        1024     311.82ns      72.51ns     notSupport       6.19ns     271.23ns        18920
SSetsDs        2048     332.95ns      75.19ns     notSupport       7.47ns     286.04ns        37608
SSetsDs        4096     358.27ns      77.92ns     notSupport       8.12ns     332.07ns        74984
SSetsDs        8192     389.49ns      84.87ns     notSupport       8.44ns     335.51ns       149736
SSetsDs       16384     494.47ns      90.21ns     notSupport       8.17ns     454.00ns       299240

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SGb_setsDs        8     220.22ns      30.06ns     notSupport      24.70ns      53.56ns          288
SGb_setsDs       16     222.76ns      25.73ns     notSupport      15.94ns      37.63ns          544
SGb_setsDs       32     319.62ns      24.56ns     notSupport      12.60ns      69.28ns         1056
SGb_setsDs       64     420.78ns      40.81ns     notSupport      11.10ns      57.33ns         2080
SGb_setsDs      128     483.60ns      48.57ns     notSupport      10.41ns      69.06ns         4128
SGb_setsDs      256     528.71ns      54.63ns     notSupport       9.81ns      58.16ns         8224
SGb_setsDs      516     556.78ns      56.08ns     notSupport       9.69ns      56.00ns        16544
SGb_setsDs     1024     663.35ns      56.50ns     notSupport       9.74ns      64.98ns        32800
SGb_setsDs     2048     743.10ns      57.60ns     notSupport       9.63ns      82.31ns        65568
SGb_setsDs     4096     808.77ns      61.40ns     notSupport       9.44ns      69.73ns       131104
SGb_setsDs     8192     870.94ns      66.28ns     notSupport       9.46ns      69.83ns       262176
SGb_setsDs    16384     990.30ns      70.04ns     notSupport       9.28ns      77.54ns       524320

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SOrddictDs        8      32.13ns      36.14ns        57.15ns      25.30ns      48.66ns          328
SOrddictDs       16      24.63ns      41.82ns       128.99ns      16.73ns     134.07ns          648
SOrddictDs       32      20.21ns      78.10ns       237.42ns      13.75ns     153.94ns         1288
SOrddictDs       64      49.03ns     146.39ns       351.29ns      11.59ns     269.47ns         2568
SOrddictDs      128      61.22ns     283.96ns       627.02ns      10.50ns     457.06ns         5128
SOrddictDs      256      65.79ns     553.89ns       992.84ns       9.78ns     872.44ns        10248
SOrddictDs      516      70.17ns    1099.30ns      1807.10ns       9.72ns    1702.18ns        20648
SOrddictDs     1024      63.14ns    2150.55ns      3607.37ns       9.34ns    3584.99ns        40968
SOrddictDs     2048      51.80ns    4267.99ns      7715.94ns       9.13ns    7260.83ns        81928
SOrddictDs     4096      48.35ns    8473.76ns     15226.90ns       9.28ns   14860.94ns       163848
SOrddictDs     8192      52.53ns   17168.91ns     31107.94ns       9.54ns   30363.49ns       327688
SOrddictDs    16384      57.55ns   34194.53ns     65373.18ns       9.51ns   60939.59ns       655368

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SOrdsetsDs        8      27.38ns      27.19ns     notSupport      23.00ns      43.19ns          136
SOrdsetsDs       16      19.98ns      35.04ns     notSupport      14.26ns     124.22ns          264
SOrdsetsDs       32      15.82ns      61.79ns     notSupport       9.35ns     174.32ns          520
SOrdsetsDs       64      13.82ns     117.49ns     notSupport       6.95ns     313.51ns         1032
SOrdsetsDs      128      30.12ns     231.69ns     notSupport       5.53ns     493.38ns         2056
SOrdsetsDs      256      37.88ns     496.77ns     notSupport       4.71ns     911.03ns         4104
SOrdsetsDs      516      51.98ns     961.07ns     notSupport       4.17ns    1602.05ns         8264
SOrdsetsDs     1024      37.28ns    1805.08ns     notSupport       4.35ns    3176.49ns        16392
SOrdsetsDs     2048      29.67ns    3715.28ns     notSupport       4.03ns    6729.36ns        32776
SOrdsetsDs     4096      27.47ns    6865.62ns     notSupport       4.10ns   13948.07ns        65544
SOrdsetsDs     8192      23.78ns   13431.76ns     notSupport       4.05ns   28702.16ns       131080
SOrdsetsDs    16384      24.92ns   27900.68ns     notSupport       4.03ns   58707.05ns       262152
ok
