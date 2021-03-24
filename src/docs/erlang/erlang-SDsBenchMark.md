DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SPdDs             8      96.11ns      17.56ns        28.04ns      62.70ns      34.69ns       noSize
SPdDs            16      78.67ns      13.75ns        19.52ns      50.01ns      23.99ns       noSize
SPdDs            32      53.33ns       9.59ns        16.05ns      92.72ns      24.80ns       noSize
SPdDs            64      80.27ns       7.22ns        13.91ns      64.54ns      21.60ns       noSize
SPdDs           128      77.41ns       6.14ns        12.89ns      64.47ns      18.56ns       noSize
SPdDs           256      72.15ns       5.58ns        12.40ns      54.64ns      16.25ns       noSize
SPdDs           516      68.23ns       5.36ns        12.25ns      56.88ns      14.84ns       noSize
SPdDs          1024      82.08ns       5.27ns        12.65ns      54.47ns      13.71ns       noSize
SPdDs          2048      69.26ns       5.01ns        11.96ns      59.97ns      13.00ns       noSize
SPdDs          4096      64.42ns       5.03ns        11.62ns      54.18ns      12.31ns       noSize
SPdDs          8192      60.40ns       4.94ns        11.49ns      52.99ns      12.16ns       noSize
SPdDs         16384      69.15ns       5.01ns        11.47ns      60.28ns      12.17ns       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
STupleDs          8      38.70ns      15.43ns        30.79ns      14.75ns   notSupport           80
STupleDs         16      87.56ns       9.75ns        68.46ns       9.89ns   notSupport          144
STupleDs         32      98.34ns       6.58ns        44.23ns       6.52ns   notSupport          272
STupleDs         64      95.04ns       5.05ns        68.91ns       5.09ns   notSupport          528
STupleDs        128     104.47ns       4.28ns       102.99ns       4.34ns   notSupport         1040
STupleDs        256     181.88ns       3.93ns       179.47ns       4.01ns   notSupport         2064
STupleDs        516     429.89ns       3.64ns       396.74ns       3.65ns   notSupport         4144
STupleDs       1024     577.22ns       3.53ns       560.16ns       3.57ns   notSupport         8208
STupleDs       2048    1144.29ns       3.38ns      1121.35ns       3.56ns   notSupport        16400
STupleDs       4096    2004.85ns       3.41ns      2023.29ns       3.39ns   notSupport        32784
STupleDs       8192    4622.60ns       3.36ns      4591.52ns       3.47ns   notSupport        65552
STupleDs      16384    8221.04ns       3.40ns      8203.76ns       3.44ns   notSupport       131088

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SListsDs          8      21.29ns      46.34ns        65.42ns      13.40ns      90.88ns          328
SListsDs         16      14.41ns      35.93ns       147.59ns       7.21ns     146.95ns          648
SListsDs         32      10.24ns      44.66ns       227.51ns       5.39ns     151.92ns         1288
SListsDs         64      29.38ns      74.58ns       362.82ns       4.05ns     306.02ns         2568
SListsDs        128      50.53ns     132.57ns       554.62ns       3.63ns     490.50ns         5128
SListsDs        256      46.15ns     210.36ns       899.96ns       3.44ns     860.89ns        10248
SListsDs        516      49.32ns     514.67ns      1839.67ns       3.20ns    1601.14ns        20648
SListsDs       1024      42.18ns     990.36ns      3313.01ns       3.82ns    3468.66ns        40968
SListsDs       2048      39.86ns    2077.25ns      7005.71ns       3.90ns    7112.97ns        81928
SListsDs       4096      47.23ns    4427.47ns     13986.89ns       4.16ns   14156.81ns       163848
SListsDs       8192      48.08ns    8953.83ns     28569.39ns       4.67ns   28016.05ns       327688
SListsDs      16384      43.38ns   17863.35ns     56614.41ns       4.65ns   59513.10ns       655368

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SMapsDs           8      54.06ns      26.11ns        97.41ns     223.25ns      42.73ns          168
SMapsDs          16     129.31ns      20.51ns        89.16ns      26.96ns      81.92ns          296
SMapsDs          32     138.17ns      21.77ns        88.00ns      20.51ns      40.30ns          552
SMapsDs          64     159.63ns      23.07ns       131.80ns      24.32ns      87.53ns         1864
SMapsDs         128     174.47ns      24.60ns       162.94ns      19.65ns     118.64ns         3640
SMapsDs         256     241.67ns      27.48ns       210.12ns      22.11ns     137.26ns         7576
SMapsDs         516     213.95ns      27.07ns       252.57ns      23.08ns     141.07ns        15432
SMapsDs        1024     277.99ns      24.84ns       233.16ns      37.71ns     159.69ns        30360
SMapsDs        2048     266.18ns      25.88ns       308.09ns      30.31ns     147.21ns        59736
SMapsDs        4096     275.68ns      29.18ns       298.96ns      30.62ns     145.43ns       121864
SMapsDs        8192     305.56ns      31.09ns       316.30ns      34.07ns     154.97ns       247848
SMapsDs       16384     320.95ns      36.52ns       307.99ns      28.61ns     182.73ns       485944

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SEtsSetDs         8     165.88ns      76.90ns        77.69ns     256.71ns     127.92ns       noSize
SEtsSetDs        16     144.95ns      65.54ns        66.63ns     272.73ns     108.73ns       noSize
SEtsSetDs        32     133.43ns      98.94ns        61.08ns     176.79ns     102.91ns       noSize
SEtsSetDs        64     135.05ns      91.20ns        58.09ns     137.70ns      99.09ns       noSize
SEtsSetDs       128     143.93ns      68.10ns        60.38ns     123.53ns     101.18ns       noSize
SEtsSetDs       256     179.82ns      65.40ns        58.54ns     118.89ns     104.53ns       noSize
SEtsSetDs       516     159.57ns      64.38ns        59.03ns     111.02ns     104.50ns       noSize
SEtsSetDs      1024     183.04ns      63.24ns        59.61ns     120.20ns     105.17ns       noSize
SEtsSetDs      2048     188.94ns      61.84ns        59.09ns     107.33ns     104.05ns       noSize
SEtsSetDs      4096     183.45ns      63.49ns        58.99ns     110.88ns     104.31ns       noSize
SEtsSetDs      8192     202.40ns      63.40ns        59.94ns     114.35ns      98.85ns       noSize
SEtsSetDs     16384     186.05ns      64.72ns        64.90ns     119.52ns     104.09ns       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SEtsOrdDs         8     173.71ns      87.06ns        75.70ns     223.56ns     124.92ns       noSize
SEtsOrdDs        16     154.78ns      73.14ns        66.34ns     243.94ns     113.63ns       noSize
SEtsOrdDs        32     151.88ns     113.07ns        63.35ns     168.49ns     112.68ns       noSize
SEtsOrdDs        64     158.27ns     107.82ns        62.78ns     133.92ns     113.98ns       noSize
SEtsOrdDs       128     161.71ns      82.67ns        71.36ns     121.78ns     112.21ns       noSize
SEtsOrdDs       256     162.07ns      80.68ns        73.65ns     115.26ns     116.12ns       noSize
SEtsOrdDs       516     197.16ns      83.30ns        79.20ns     122.70ns     123.81ns       noSize
SEtsOrdDs      1024     170.64ns      78.54ns        83.92ns     115.37ns     114.58ns       noSize
SEtsOrdDs      2048     173.82ns      81.86ns        77.51ns     110.43ns     106.66ns       noSize
SEtsOrdDs      4096     174.13ns      82.12ns        79.64ns     109.21ns     113.78ns       noSize
SEtsOrdDs      8192     173.87ns      83.79ns        82.10ns     109.42ns     105.17ns       noSize
SEtsOrdDs     16384     169.23ns      86.04ns        89.24ns     115.50ns     108.25ns       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SArrayDs          8      73.00ns      27.41ns       203.06ns      27.46ns   notSupport          144
SArrayDs         16     205.46ns      37.37ns       178.76ns      34.48ns   notSupport          328
SArrayDs         32     166.79ns      31.91ns       124.24ns      30.62ns   notSupport          504
SArrayDs         64     149.59ns      29.90ns        96.45ns      28.90ns   notSupport          768
SArrayDs        128     152.25ns      49.47ns       130.15ns      49.33ns   notSupport         1488
SArrayDs        256     148.64ns      50.66ns       154.59ns      48.74ns   notSupport         2728
SArrayDs        516     168.27ns      48.53ns       141.45ns      48.39ns   notSupport         5304
SArrayDs       1024     221.77ns      66.96ns       183.80ns      68.72ns   notSupport        10464
SArrayDs       2048     191.58ns      67.04ns       165.01ns      66.67ns   notSupport        20496
SArrayDs       4096     176.87ns      69.13ns       170.46ns      66.92ns   notSupport        40648
SArrayDs       8192     172.83ns      69.22ns       161.44ns      66.57ns   notSupport        81048
SArrayDs      16384     201.58ns      85.30ns       201.62ns      85.93ns   notSupport       161952

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SDictDs           8     506.85ns      96.84ns       383.52ns      57.40ns     166.41ns          632
SDictDs          16     372.72ns      85.94ns       311.05ns      33.02ns     196.36ns          888
SDictDs          32     331.65ns      84.37ns       248.66ns      22.58ns     176.09ns         1400
SDictDs          64     297.77ns      86.92ns       259.65ns      17.40ns     205.56ns         2424
SDictDs         128     430.22ns     101.62ns       355.91ns      15.83ns     231.80ns         4616
SDictDs         256     511.98ns     101.44ns       283.77ns      15.50ns     232.83ns         9000
SDictDs         516     429.74ns      99.89ns       267.24ns      15.64ns     250.21ns        17896
SDictDs        1024     458.51ns      91.52ns       299.80ns      15.19ns     245.51ns        35304
SDictDs        2048     472.67ns      96.20ns       333.20ns      15.32ns     239.61ns        70376
SDictDs        4096     508.30ns     106.40ns       334.23ns      16.45ns     253.70ns       140520
SDictDs        8192     573.14ns     122.50ns       371.37ns      17.57ns     292.14ns       280808
SDictDs       16384     668.01ns     134.43ns       403.07ns      18.09ns     328.55ns       561384

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SGb_treesD        8     266.29ns      36.41ns       135.54ns      26.68ns     106.83ns          352
SGb_treesD       16     265.97ns      29.41ns       117.86ns      50.64ns      56.77ns          672
SGb_treesD       32     413.88ns      50.33ns       121.17ns      16.93ns      69.35ns         1312
SGb_treesD       64     428.31ns      52.02ns       120.76ns      28.98ns      51.50ns         2592
SGb_treesD      128     503.03ns      54.94ns       131.97ns      13.86ns      56.38ns         5152
SGb_treesD      256     541.01ns      65.32ns       129.09ns      13.44ns      61.73ns        10272
SGb_treesD      516     573.12ns      59.61ns       130.51ns      42.36ns      85.01ns        20672
SGb_treesD     1024     683.50ns      63.05ns       166.46ns      23.29ns      65.11ns        40992
SGb_treesD     2048     733.85ns      61.25ns       147.67ns      31.28ns      72.15ns        81952
SGb_treesD     4096     802.63ns      65.38ns       151.52ns      13.61ns     109.18ns       163872
SGb_treesD     8192     853.50ns      71.30ns       171.56ns      35.14ns      76.65ns       327712
SGb_treesD    16384     944.83ns      73.57ns       151.66ns      49.24ns     139.47ns       655392

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SSetsDs           8     340.18ns      86.26ns     notSupport      59.51ns     312.85ns          504
SSetsDs          16     292.73ns      80.44ns     notSupport      31.56ns     260.52ns          632
SSetsDs          32     265.59ns      77.51ns     notSupport      20.36ns     203.26ns          888
SSetsDs          64     228.80ns      79.64ns     notSupport      14.79ns     192.42ns         1400
SSetsDs         128     245.56ns      88.48ns     notSupport      14.10ns     272.98ns         2568
SSetsDs         256     329.50ns      85.99ns     notSupport      13.11ns     280.72ns         4904
SSetsDs         516     351.88ns      79.12ns     notSupport      13.00ns     293.16ns         9640
SSetsDs        1024     346.45ns      77.43ns     notSupport      11.96ns     292.20ns        18920
SSetsDs        2048     360.19ns      83.63ns     notSupport      12.68ns     319.85ns        37608
SSetsDs        4096     397.59ns      88.48ns     notSupport      12.68ns     334.74ns        74984
SSetsDs        8192     429.84ns      90.46ns     notSupport      11.89ns     390.99ns       149736
SSetsDs       16384     501.32ns      95.30ns     notSupport      12.49ns     429.00ns       299240

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SGb_setsDs        8     258.80ns      31.74ns     notSupport      33.20ns      49.06ns          288
SGb_setsDs       16     254.54ns      30.89ns     notSupport      23.10ns      39.74ns          544
SGb_setsDs       32     357.38ns      36.25ns     notSupport      18.33ns      76.03ns         1056
SGb_setsDs       64     442.09ns      49.27ns     notSupport      16.78ns      60.04ns         2080
SGb_setsDs      128     519.47ns      53.38ns     notSupport      15.75ns      72.34ns         4128
SGb_setsDs      256     589.81ns      58.32ns     notSupport      14.87ns      63.58ns         8224
SGb_setsDs      516     645.86ns      63.16ns     notSupport      14.24ns      61.57ns        16544
SGb_setsDs     1024     724.70ns      59.88ns     notSupport      14.49ns      66.62ns        32800
SGb_setsDs     2048     770.31ns      62.81ns     notSupport      23.13ns      74.70ns        65568
SGb_setsDs     4096     833.23ns      66.72ns     notSupport      13.83ns      71.57ns       131104
SGb_setsDs     8192     905.41ns      70.15ns     notSupport      13.59ns      72.01ns       262176
SGb_setsDs    16384    1013.93ns      74.37ns     notSupport      13.64ns      78.78ns       524320

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SOrddictDs        8      33.89ns      34.24ns        57.13ns      34.45ns      53.58ns          328
SOrddictDs       16      23.79ns      46.76ns       131.06ns      24.24ns     131.35ns          648
SOrddictDs       32      20.58ns      84.14ns       244.88ns      18.57ns     156.40ns         1288
SOrddictDs       64      44.83ns     155.48ns       385.99ns      15.69ns     301.76ns         2568
SOrddictDs      128      47.43ns     367.32ns       592.42ns      14.11ns     505.11ns         5128
SOrddictDs      256      78.57ns     678.40ns       952.24ns      13.00ns     854.72ns        10248
SOrddictDs      516      75.48ns    1133.54ns      1979.42ns      12.56ns    1642.34ns        20648
SOrddictDs     1024      50.27ns    2220.68ns      3703.02ns      12.34ns    3568.94ns        40968
SOrddictDs     2048      49.53ns    4372.61ns      7451.61ns      12.16ns    7489.62ns        81928
SOrddictDs     4096      58.01ns    8687.54ns     15202.68ns      12.40ns   14562.63ns       163848
SOrddictDs     8192      54.12ns   17275.72ns     31375.12ns      12.51ns   29705.30ns       327688
SOrddictDs    16384      49.89ns   34411.87ns     62431.91ns      12.64ns   62998.68ns       655368

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SOrdsetsDs        8      25.50ns      32.69ns     notSupport      35.04ns      45.64ns          136
SOrdsetsDs       16      19.91ns      41.72ns     notSupport      21.47ns     122.74ns          264
SOrdsetsDs       32      16.84ns      75.07ns     notSupport      14.68ns     174.21ns          520
SOrdsetsDs       64      14.35ns     126.19ns     notSupport      11.11ns     307.74ns         1032
SOrdsetsDs      128      20.69ns     266.95ns     notSupport       9.40ns     586.44ns         2056
SOrdsetsDs      256      21.19ns     583.81ns     notSupport       8.24ns     951.64ns         4104
SOrdsetsDs      516      28.14ns     988.00ns     notSupport       7.58ns    1662.64ns         8264
SOrdsetsDs     1024      30.86ns    1681.40ns     notSupport       7.66ns    3253.57ns        16392
SOrdsetsDs     2048      26.36ns    3730.25ns     notSupport       7.54ns    6978.50ns        32776
SOrdsetsDs     4096      25.88ns    7145.89ns     notSupport       7.19ns   13961.94ns        65544
SOrdsetsDs     8192      25.91ns   14951.49ns     notSupport       7.39ns   28558.85ns       131080
SOrdsetsDs    16384      23.65ns   28407.73ns     notSupport       7.17ns   58353.48ns       262152
ok
