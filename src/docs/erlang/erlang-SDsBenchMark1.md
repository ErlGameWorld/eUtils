DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SPdDs             8      80.13ns      19.98ns        26.35ns      63.54ns      34.70ns       noSize
SPdDs            16      63.62ns      11.57ns        17.93ns      41.24ns      21.06ns       noSize
SPdDs            32      44.22ns       8.38ns        14.96ns      83.52ns      20.37ns       noSize
SPdDs            64      74.69ns       6.83ns        13.11ns      62.08ns      19.46ns       noSize
SPdDs           128      74.70ns       5.89ns        12.26ns      61.83ns      16.32ns       noSize
SPdDs           256      70.21ns       5.42ns        12.23ns      52.02ns      14.88ns       noSize
SPdDs           516      64.96ns       5.23ns        12.04ns      51.18ns      13.98ns       noSize
SPdDs          1024      69.11ns       5.13ns        12.81ns      49.87ns      12.97ns       noSize
SPdDs          2048      56.03ns       5.09ns        12.22ns      54.59ns      12.39ns       noSize
SPdDs          4096      55.63ns       5.24ns        11.58ns      53.83ns      12.23ns       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
STupleDs          8      39.02ns      16.45ns        30.01ns      15.59ns   notSupport           80
STupleDs         16      94.52ns      10.67ns        69.25ns      10.13ns   notSupport          144
STupleDs         32     108.77ns       7.18ns        42.66ns       6.91ns   notSupport          272
STupleDs         64      93.72ns       5.23ns        67.22ns       5.18ns   notSupport          528
STupleDs        128     111.16ns       4.40ns       108.54ns       4.36ns   notSupport         1040
STupleDs        256     180.01ns       3.95ns       174.05ns       4.02ns   notSupport         2064
STupleDs        516     359.42ns       3.64ns       339.78ns       3.57ns   notSupport         4144
STupleDs       1024     527.74ns       3.47ns       509.43ns       3.44ns   notSupport         8208
STupleDs       2048    1112.67ns       3.32ns      1092.55ns       3.53ns   notSupport        16400
STupleDs       4096    1993.53ns       3.46ns      1994.36ns       3.41ns   notSupport        32784

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SListsDs          8      17.02ns      35.56ns        54.48ns      12.61ns      50.80ns          328
SListsDs         16      12.31ns      31.74ns       125.96ns       8.31ns     137.08ns          648
SListsDs         32       9.73ns      41.02ns       226.87ns       5.67ns     152.55ns         1288
SListsDs         64      28.85ns      72.18ns       368.05ns       4.57ns     297.92ns         2568
SListsDs        128      50.87ns     128.78ns       561.09ns       3.74ns     481.60ns         5128
SListsDs        256      61.23ns     251.98ns       886.71ns       3.48ns     822.72ns        10248
SListsDs        516      68.01ns     456.31ns      1844.14ns       3.34ns    1576.32ns        20648
SListsDs       1024      46.12ns     969.88ns      3335.85ns       4.14ns    3474.52ns        40968
SListsDs       2048      40.00ns    2079.84ns      6887.37ns       3.82ns    7109.81ns        81928
SListsDs       4096      44.93ns    4328.40ns     13819.91ns       3.91ns   13615.04ns       163848

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SMapsDs           8      54.40ns      23.98ns        51.30ns     209.22ns      45.83ns          168
SMapsDs          16     134.34ns      17.34ns        91.29ns      31.56ns      77.61ns          296
SMapsDs          32     142.40ns      18.99ns        93.63ns      22.43ns      40.18ns          552
SMapsDs          64     159.52ns      24.30ns       138.39ns      24.09ns      80.24ns         1976
SMapsDs         128     164.44ns      23.15ns       169.39ns      20.31ns     113.94ns         3736
SMapsDs         256     176.24ns      26.18ns       148.01ns      22.02ns     121.37ns         7528
SMapsDs         516     187.36ns      26.40ns       224.19ns      61.37ns     132.19ns        15560
SMapsDs        1024     208.07ns      23.51ns       295.84ns      38.56ns     142.12ns        30392
SMapsDs        2048     262.29ns      25.09ns       267.79ns      26.48ns     185.66ns        59896
SMapsDs        4096     280.60ns      28.36ns       298.79ns      28.84ns     138.76ns       122648

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SQueueDs          8      28.40ns      72.75ns     notSupport   notSupport   notSupport          160
SQueueDs         16      23.69ns     123.45ns     notSupport   notSupport   notSupport          288
SQueueDs         32      18.55ns     102.87ns     notSupport   notSupport   notSupport          544
SQueueDs         64      27.61ns      75.82ns     notSupport   notSupport   notSupport         1056
SQueueDs        128      30.60ns      68.49ns     notSupport   notSupport   notSupport         2080
SQueueDs        256      36.99ns      50.56ns     notSupport   notSupport   notSupport         4128
SQueueDs        516      28.62ns      56.89ns     notSupport   notSupport   notSupport         8288
SQueueDs       1024      28.97ns      54.38ns     notSupport   notSupport   notSupport        16416
SQueueDs       2048      29.66ns      53.49ns     notSupport   notSupport   notSupport        32800
SQueueDs       4096      25.83ns      63.59ns     notSupport   notSupport   notSupport        65568

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SUtRiroDs         8     296.41ns     386.44ns     notSupport   notSupport   notSupport          117
SUtRiroDs        16     261.76ns     364.24ns     notSupport   notSupport   notSupport          117
SUtRiroDs        32     245.98ns     348.34ns     notSupport   notSupport   notSupport          117
SUtRiroDs        64     241.25ns     367.00ns     notSupport   notSupport   notSupport          117
SUtRiroDs       128     245.70ns     365.11ns     notSupport   notSupport   notSupport          117
SUtRiroDs       256     250.23ns     368.19ns     notSupport   notSupport   notSupport          117
SUtRiroDs       516     242.42ns     373.04ns     notSupport   notSupport   notSupport          117
SUtRiroDs      1024     245.76ns     370.70ns     notSupport   notSupport   notSupport          117
SUtRiroDs      2048     249.26ns     374.47ns     notSupport   notSupport   notSupport          117
SUtRiroDs      4096     257.05ns     371.40ns     notSupport   notSupport   notSupport          117

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SUtLifoDs         8     317.24ns     268.39ns     notSupport   notSupport   notSupport         1329
SUtLifoDs        16     275.92ns     244.39ns     notSupport   notSupport   notSupport         1329
SUtLifoDs        32     262.41ns     265.32ns     notSupport   notSupport   notSupport         1329
SUtLifoDs        64     255.59ns     259.48ns     notSupport   notSupport   notSupport         1329
SUtLifoDs       128     258.81ns     247.60ns     notSupport   notSupport   notSupport         1329
SUtLifoDs       256     260.08ns     250.77ns     notSupport   notSupport   notSupport         1329
SUtLifoDs       516     280.68ns     251.70ns     notSupport   notSupport   notSupport         3639
SUtLifoDs      1024     305.45ns     255.90ns     notSupport   notSupport   notSupport         3639
SUtLifoDs      2048     310.07ns     257.11ns     notSupport   notSupport   notSupport         3639
SUtLifoDs      4096     303.32ns     276.50ns     notSupport   notSupport   notSupport         3639

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SEtsSetDs         8     170.68ns      78.76ns        76.22ns     253.88ns     122.14ns          305
SEtsSetDs        16     145.07ns      61.96ns        62.01ns     257.71ns     104.42ns          305
SEtsSetDs        32     127.97ns      95.35ns        56.69ns     158.26ns      95.27ns          305
SEtsSetDs        64     121.14ns      85.60ns        53.82ns     126.68ns      94.26ns          305
SEtsSetDs       128     123.89ns      64.25ns        55.57ns     113.20ns      90.72ns          305
SEtsSetDs       256     127.07ns      60.45ns        54.83ns     104.84ns      91.83ns          305
SEtsSetDs       516     137.46ns      57.78ns        54.82ns     102.99ns      92.36ns         2615
SEtsSetDs      1024     141.47ns      57.49ns        53.41ns     109.25ns     105.95ns         2615
SEtsSetDs      2048     146.04ns      57.43ns        53.97ns     106.56ns     106.78ns         2615
SEtsSetDs      4096     158.02ns      57.63ns        55.51ns     103.48ns     109.00ns         2615

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SEtsOrdDs         8     174.38ns      77.72ns        77.21ns     201.61ns     113.06ns          139
SEtsOrdDs        16     145.09ns      64.18ns        58.92ns     223.09ns     102.91ns          139
SEtsOrdDs        32     137.32ns     103.13ns        56.42ns     140.78ns      98.75ns          139
SEtsOrdDs        64     135.51ns      98.40ns        58.39ns     121.06ns     104.44ns          139
SEtsOrdDs       128     140.88ns      76.99ns        65.24ns     110.19ns     101.41ns          139
SEtsOrdDs       256     142.73ns      73.86ns        68.06ns     105.57ns     102.80ns          139
SEtsOrdDs       516     142.01ns      73.25ns        69.11ns     104.67ns     102.00ns          139
SEtsOrdDs      1024     144.61ns      74.09ns        70.48ns     109.86ns     112.50ns          139
SEtsOrdDs      2048     151.13ns      76.35ns        71.70ns     106.10ns     112.72ns          139
SEtsOrdDs      4096     150.29ns      77.59ns        74.27ns     103.33ns     113.56ns          139

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SArrayDs          8      73.42ns      32.90ns       198.03ns      25.91ns   notSupport          144
SArrayDs         16     217.84ns      37.08ns       186.88ns      35.46ns   notSupport          328
SArrayDs         32     173.94ns      31.55ns       123.50ns      30.47ns   notSupport          504
SArrayDs         64     157.17ns      29.36ns        97.65ns      28.87ns   notSupport          768
SArrayDs        128     155.00ns      49.81ns       131.44ns      48.97ns   notSupport         1488
SArrayDs        256     144.80ns      49.11ns       145.37ns      47.57ns   notSupport         2728
SArrayDs        516     142.70ns      46.55ns       136.24ns      46.69ns   notSupport         5304
SArrayDs       1024     185.99ns      66.72ns       177.52ns      66.45ns   notSupport        10464
SArrayDs       2048     186.80ns      67.75ns       164.59ns      65.80ns   notSupport        20496
SArrayDs       4096     174.53ns      65.26ns       166.03ns      65.05ns   notSupport        40648

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SDictDs           8     679.58ns      93.83ns       611.73ns      58.69ns     164.28ns          632
SDictDs          16     357.09ns      81.05ns       306.51ns      31.82ns     178.94ns          888
SDictDs          32     308.13ns      79.40ns       231.73ns      21.88ns     169.57ns         1400
SDictDs          64     278.26ns      83.68ns       249.01ns      17.16ns     201.38ns         2424
SDictDs         128     352.86ns      96.46ns       265.91ns      16.39ns     243.84ns         4616
SDictDs         256     409.38ns      98.76ns       270.41ns      15.54ns     230.16ns         9000
SDictDs         516     412.71ns      98.04ns       257.45ns      15.24ns     241.61ns        17896
SDictDs        1024     462.56ns      92.94ns       344.93ns      15.21ns     247.46ns        35304
SDictDs        2048     488.54ns      96.59ns       367.79ns      14.96ns     236.98ns        70376
SDictDs        4096     503.00ns     106.03ns       325.94ns      16.02ns     249.15ns       140520

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SGb_treesD        8     282.60ns      31.91ns       133.26ns      26.57ns     108.51ns          352
SGb_treesD       16     256.45ns      26.63ns       103.55ns      47.68ns      52.52ns          672
SGb_treesD       32     394.83ns      48.48ns       116.34ns      16.19ns      61.93ns         1312
SGb_treesD       64     408.46ns      50.54ns       114.08ns      27.13ns      49.88ns         2592
SGb_treesD      128     509.10ns      53.13ns       125.98ns      13.18ns      53.76ns         5152
SGb_treesD      256     514.36ns      63.00ns       117.81ns      12.70ns      57.49ns        10272
SGb_treesD      516     528.76ns      58.07ns       125.12ns      41.24ns      79.65ns        20672
SGb_treesD     1024     640.81ns      60.32ns       158.70ns      23.07ns      64.59ns        40992
SGb_treesD     2048     727.13ns      60.68ns       141.84ns      29.51ns      70.19ns        81952
SGb_treesD     4096     806.84ns      63.67ns       145.88ns      13.27ns     103.56ns       163872

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SSetsDs           8     393.74ns      85.67ns     notSupport      56.49ns     321.09ns          504
SSetsDs          16     281.31ns      76.27ns     notSupport      31.20ns     252.72ns          632
SSetsDs          32     253.70ns      74.03ns     notSupport      20.01ns     188.58ns          888
SSetsDs          64     214.41ns      74.67ns     notSupport      16.77ns     181.68ns         1400
SSetsDs         128     231.77ns      84.16ns     notSupport      14.24ns     267.05ns         2568
SSetsDs         256     286.15ns      79.78ns     notSupport      13.43ns     272.14ns         4904
SSetsDs         516     343.54ns      74.99ns     notSupport      13.41ns     272.29ns         9640
SSetsDs        1024     314.23ns      74.81ns     notSupport      12.88ns     279.45ns        18920
SSetsDs        2048     352.26ns      79.73ns     notSupport      12.20ns     301.65ns        37608
SSetsDs        4096     383.00ns      83.49ns     notSupport      12.09ns     319.93ns        74984

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SGb_setsDs        8     242.51ns      35.94ns     notSupport      36.89ns      57.39ns          288
SGb_setsDs       16     261.44ns      28.56ns     notSupport      25.24ns      40.80ns          544
SGb_setsDs       32     355.53ns      34.42ns     notSupport      19.84ns      76.98ns         1056
SGb_setsDs       64     450.91ns      49.35ns     notSupport      17.28ns      61.86ns         2080
SGb_setsDs      128     517.83ns      54.74ns     notSupport      16.09ns      71.94ns         4128
SGb_setsDs      256     576.78ns      56.76ns     notSupport      14.96ns      61.76ns         8224
SGb_setsDs      516     580.58ns      58.96ns     notSupport      14.20ns      57.82ns        16544
SGb_setsDs     1024     674.71ns      58.60ns     notSupport      14.29ns      64.39ns        32800
SGb_setsDs     2048     756.15ns      61.41ns     notSupport      23.88ns      76.26ns        65568
SGb_setsDs     4096     817.23ns      63.75ns     notSupport      13.50ns      69.41ns       131104

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SOrddictDs        8      31.75ns      38.48ns        58.54ns      32.70ns      49.98ns          328
SOrddictDs       16      24.97ns      50.63ns       208.51ns      21.38ns     129.39ns          648
SOrddictDs       32      20.79ns      87.60ns       250.23ns      18.31ns     174.71ns         1288
SOrddictDs       64      38.35ns     156.47ns       404.38ns      15.60ns     316.40ns         2568
SOrddictDs      128      49.51ns     327.63ns       582.88ns      14.00ns     514.27ns         5128
SOrddictDs      256      82.46ns     608.43ns       974.28ns      12.92ns     879.01ns        10248
SOrddictDs      516      82.69ns    1123.62ns      1939.18ns      12.36ns    1698.00ns        20648
SOrddictDs     1024      56.43ns    2159.33ns      3586.33ns      12.03ns    3534.09ns        40968
SOrddictDs     2048      49.15ns    4319.18ns      7393.47ns      12.18ns    7444.38ns        81928
SOrddictDs     4096      59.69ns    8571.96ns     15119.75ns      12.28ns   14509.69ns       163848

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SOrdsetsDs        8      27.07ns      35.81ns     notSupport      40.24ns      49.45ns          136
SOrdsetsDs       16      21.24ns      47.23ns     notSupport      24.03ns     139.30ns          264
SOrdsetsDs       32      16.95ns      76.77ns     notSupport      16.35ns     265.48ns          520
SOrdsetsDs       64      14.66ns     137.42ns     notSupport      12.04ns     330.05ns         1032
SOrdsetsDs      128      21.88ns     267.31ns     notSupport       9.78ns     560.25ns         2056
SOrdsetsDs      256      27.93ns     526.30ns     notSupport       8.24ns     893.81ns         4104
SOrdsetsDs      516      27.65ns     886.36ns     notSupport       7.62ns    1718.89ns         8264
SOrdsetsDs     1024      38.68ns    1686.27ns     notSupport       7.56ns    3339.04ns        16392
SOrdsetsDs     2048      28.05ns    3726.41ns     notSupport       7.27ns    7092.48ns        32776
SOrdsetsDs     4096      25.71ns    7038.59ns     notSupport       7.36ns   14109.32ns        65544
ok
