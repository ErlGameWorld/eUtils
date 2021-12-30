otp24.1.3
DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SPdDs             8      96.75ns      20.66ns        24.52ns      51.08ns      33.04ns       noSize
SPdDs            16      57.21ns      13.73ns        18.02ns      42.81ns      23.77ns       noSize
SPdDs            32      44.96ns       9.72ns        15.32ns      84.36ns      22.41ns       noSize
SPdDs            64      73.47ns       7.41ns        13.64ns      65.27ns      19.98ns       noSize
SPdDs           128      75.56ns       6.24ns        12.97ns      65.40ns      17.55ns       noSize
SPdDs           256      69.78ns       5.78ns        12.58ns      55.27ns      15.77ns       noSize
SPdDs           516      65.90ns       5.59ns        12.25ns      55.10ns      14.99ns       noSize
SPdDs          1024      72.43ns       5.41ns        12.99ns      52.99ns      13.71ns       noSize
SPdDs          2048      65.17ns       6.47ns        11.69ns      55.60ns      12.75ns       noSize
SPdDs          4096      59.85ns       5.10ns        11.17ns      51.09ns      12.40ns       noSize
SPdDs          8192      55.75ns       5.22ns        11.29ns      53.52ns      12.43ns       noSize
SPdDs         16384      63.90ns       5.09ns        11.05ns      60.07ns      12.03ns       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
STupleDs          8      40.34ns      15.13ns        31.21ns      13.91ns   notSupport           80
STupleDs         16      87.34ns      10.44ns        80.45ns       9.96ns   notSupport          144
STupleDs         32      91.40ns       6.78ns        42.37ns       6.57ns   notSupport          272
STupleDs         64      96.02ns       5.29ns        71.17ns       4.87ns   notSupport          528
STupleDs        128     134.49ns       4.48ns       114.18ns       4.32ns   notSupport         1040
STupleDs        256     197.91ns       4.03ns       210.67ns       3.76ns   notSupport         2064
STupleDs        516     392.76ns       3.74ns       351.59ns       3.62ns   notSupport         4144
STupleDs       1024     534.12ns       3.47ns       537.47ns       3.45ns   notSupport         8208
STupleDs       2048    1130.39ns       3.32ns      1119.79ns       3.49ns   notSupport        16400
STupleDs       4096    1994.12ns       3.46ns      1989.76ns       3.48ns   notSupport        32784
STupleDs       8192    4608.30ns       3.41ns      4617.07ns       3.42ns   notSupport        65552
STupleDs      16384    8392.03ns       3.47ns      8402.30ns       3.42ns   notSupport       131088

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SListsDs          8      20.11ns      37.09ns        54.71ns      11.47ns      49.79ns          328
SListsDs         16      13.68ns      32.06ns       201.51ns       7.18ns     142.28ns          648
SListsDs         32      10.59ns      43.09ns       239.43ns       5.29ns     154.98ns         1288
SListsDs         64      36.47ns      73.23ns       372.45ns       4.11ns     285.36ns         2568
SListsDs        128      41.70ns     131.91ns       557.74ns       3.65ns     466.28ns         5128
SListsDs        256      55.43ns     291.81ns       875.47ns       3.42ns     818.78ns        10248
SListsDs        516      63.63ns     464.82ns      1821.71ns       3.29ns    1572.62ns        20648
SListsDs       1024      45.51ns     974.77ns      3392.12ns       4.06ns    3604.50ns        40968
SListsDs       2048      39.54ns    2080.82ns      7045.66ns       3.94ns    7243.94ns        81928
SListsDs       4096      41.94ns    4297.06ns     14147.75ns       4.19ns   13887.59ns       163848
SListsDs       8192      44.11ns    8948.98ns     29541.44ns       4.86ns   28658.12ns       327688
SListsDs      16384      36.90ns   17746.77ns     56719.51ns       4.99ns   59046.06ns       655368

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SMapsDs           8      55.79ns      24.76ns        51.79ns     196.55ns      38.90ns          168
SMapsDs          16     137.35ns      18.10ns       103.38ns      26.55ns      81.92ns          296
SMapsDs          32     219.32ns      21.28ns        89.72ns      19.62ns      40.39ns          552
SMapsDs          64     182.69ns      25.40ns       152.01ns      24.24ns      85.51ns         1976
SMapsDs         128     177.80ns      24.53ns       170.66ns      20.39ns     117.07ns         3736
SMapsDs         256     197.99ns      26.92ns       154.15ns      22.45ns     127.10ns         7528
SMapsDs         516     213.29ns      26.75ns       245.22ns      63.67ns     138.37ns        15560
SMapsDs        1024     227.16ns      23.41ns       290.89ns      38.29ns     143.70ns        30392
SMapsDs        2048     256.54ns      25.52ns       274.00ns      26.38ns     188.41ns        59896
SMapsDs        4096     278.15ns      29.55ns       302.80ns      28.61ns     138.97ns       122648
SMapsDs        8192     309.27ns      33.46ns       337.81ns      65.26ns     170.38ns       248312
SMapsDs       16384     330.53ns      38.25ns       326.38ns      26.53ns     169.67ns       487576

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SEtsSetDs         8     176.22ns      80.06ns        78.61ns     269.31ns     125.47ns       noSize
SEtsSetDs        16     142.82ns      65.51ns        63.72ns     271.50ns     102.95ns       noSize
SEtsSetDs        32     130.95ns      99.10ns        59.48ns     186.00ns      97.76ns       noSize
SEtsSetDs        64     125.73ns      91.28ns        57.03ns     141.50ns      96.11ns       noSize
SEtsSetDs       128     122.78ns      63.66ns        55.55ns     116.01ns      89.08ns       noSize
SEtsSetDs       256     131.42ns      62.25ns        54.84ns     106.89ns      89.24ns       noSize
SEtsSetDs       516     154.37ns      61.45ns        54.95ns     104.14ns      99.78ns       noSize
SEtsSetDs      1024     153.09ns      57.22ns        52.51ns     101.75ns     101.02ns       noSize
SEtsSetDs      2048     151.75ns      58.24ns        53.49ns     102.68ns     104.19ns       noSize
SEtsSetDs      4096     157.53ns      60.02ns        56.37ns     104.87ns     110.21ns       noSize
SEtsSetDs      8192     164.49ns      61.81ns        57.40ns     106.59ns     113.93ns       noSize
SEtsSetDs     16384     168.57ns      64.20ns        63.52ns     112.64ns     118.56ns       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SEtsOrdDs         8     508.06ns      76.25ns        72.79ns     375.74ns     120.33ns       noSize
SEtsOrdDs        16     152.82ns      68.77ns        68.81ns     236.51ns     107.91ns       noSize
SEtsOrdDs        32     151.31ns     102.55ns        60.09ns     165.38ns     104.73ns       noSize
SEtsOrdDs        64     148.51ns     101.05ns        59.93ns     132.70ns     102.88ns       noSize
SEtsOrdDs       128     155.09ns      80.45ns        67.03ns     118.22ns     103.46ns       noSize
SEtsOrdDs       256     162.57ns      77.73ns        70.97ns     112.81ns     110.99ns       noSize
SEtsOrdDs       516     165.56ns      75.09ns        70.74ns     104.90ns     103.92ns       noSize
SEtsOrdDs      1024     164.58ns      73.88ns        70.75ns     102.89ns     107.38ns       noSize
SEtsOrdDs      2048     155.22ns      76.56ns        73.14ns     103.06ns     111.56ns       noSize
SEtsOrdDs      4096     152.50ns      78.61ns        75.18ns     103.28ns     114.76ns       noSize
SEtsOrdDs      8192     151.17ns      80.02ns        76.24ns     102.64ns     120.38ns       noSize
SEtsOrdDs     16384     149.00ns      80.89ns        82.72ns     104.47ns     119.79ns       noSize

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SArrayDs          8      82.47ns      29.31ns       204.71ns      26.74ns   notSupport          144
SArrayDs         16     206.52ns      36.77ns       325.58ns      34.26ns   notSupport          328
SArrayDs         32     212.02ns      31.84ns       205.03ns      30.44ns   notSupport          504
SArrayDs         64     150.91ns      29.57ns        96.35ns      28.63ns   notSupport          768
SArrayDs        128     152.41ns      49.16ns       134.36ns      49.00ns   notSupport         1488
SArrayDs        256     181.91ns      48.04ns       146.37ns      46.68ns   notSupport         2728
SArrayDs        516     142.00ns      46.50ns       137.18ns      46.36ns   notSupport         5304
SArrayDs       1024     191.98ns      66.47ns       177.95ns      66.45ns   notSupport        10464
SArrayDs       2048     186.45ns      66.40ns       164.56ns      66.56ns   notSupport        20496
SArrayDs       4096     171.11ns      65.34ns       169.71ns      65.26ns   notSupport        40648
SArrayDs       8192     171.40ns      64.89ns       161.26ns      65.75ns   notSupport        81048
SArrayDs      16384     198.92ns      85.88ns       197.74ns      84.80ns   notSupport       161952

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SDictDs           8     779.91ns      85.70ns       400.01ns      57.65ns     165.22ns          632
SDictDs          16     368.27ns      79.73ns       314.99ns      31.83ns     187.89ns          888
SDictDs          32     306.79ns      80.21ns       229.12ns      22.16ns     170.11ns         1400
SDictDs          64     282.76ns      83.41ns       250.24ns      17.10ns     195.28ns         2424
SDictDs         128     367.94ns      95.39ns       272.93ns      16.42ns     234.64ns         4616
SDictDs         256     414.36ns      99.32ns       258.40ns      15.53ns     231.58ns         9000
SDictDs         516     417.83ns      96.41ns       255.03ns      14.77ns     244.55ns        17896
SDictDs        1024     431.65ns      90.46ns       300.16ns      15.08ns     244.79ns        35304
SDictDs        2048     469.79ns      95.02ns       331.42ns      14.82ns     238.06ns        70376
SDictDs        4096     498.99ns     104.74ns       324.71ns      15.96ns     251.91ns       140520
SDictDs        8192     562.90ns     116.10ns       366.01ns      16.89ns     288.43ns       280808
SDictDs       16384     669.28ns     133.79ns       397.01ns      18.02ns     326.83ns       561384

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SGb_treesD        8     244.94ns      35.40ns       160.30ns      27.31ns     108.31ns          352
SGb_treesD       16     272.61ns      28.09ns       122.76ns      52.69ns      57.92ns          672
SGb_treesD       32     419.07ns      52.69ns       125.31ns      17.25ns      66.95ns         1312
SGb_treesD       64     430.57ns      51.02ns       121.83ns      29.50ns      51.42ns         2592
SGb_treesD      128     501.67ns      53.65ns       127.53ns      13.86ns      56.19ns         5152
SGb_treesD      256     563.95ns      65.01ns       123.15ns      13.28ns      60.98ns        10272
SGb_treesD      516     571.57ns      58.92ns       128.10ns      43.40ns      80.97ns        20672
SGb_treesD     1024     630.23ns      60.87ns       158.78ns      22.67ns      64.55ns        40992
SGb_treesD     2048     705.42ns      60.12ns       144.28ns      28.12ns      70.73ns        81952
SGb_treesD     4096     764.47ns      63.70ns       148.35ns      13.26ns     104.69ns       163872
SGb_treesD     8192     817.57ns      74.24ns       166.43ns      32.23ns      74.82ns       327712
SGb_treesD    16384     908.90ns      72.66ns       151.75ns      48.32ns     136.07ns       655392

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SSetsDs           8     336.46ns      87.45ns     notSupport      56.05ns     322.31ns          504
SSetsDs          16     309.81ns      80.66ns     notSupport      31.24ns     267.97ns          632
SSetsDs          32     270.74ns      78.52ns     notSupport      19.84ns     247.26ns          888
SSetsDs          64     231.60ns      79.55ns     notSupport      14.64ns     192.99ns         1400
SSetsDs         128     247.38ns      88.31ns     notSupport      13.78ns     280.69ns         2568
SSetsDs         256     310.74ns      81.66ns     notSupport      12.30ns     275.27ns         4904
SSetsDs         516     320.00ns      75.09ns     notSupport      12.10ns     268.46ns         9640
SSetsDs        1024     317.35ns      74.25ns     notSupport      11.37ns     278.62ns        18920
SSetsDs        2048     344.28ns      81.11ns     notSupport      12.32ns     308.13ns        37608
SSetsDs        4096     389.44ns      84.11ns     notSupport      12.35ns     320.07ns        74984
SSetsDs        8192     430.10ns      91.61ns     notSupport      12.22ns     387.75ns       149736
SSetsDs       16384     498.93ns      93.15ns     notSupport      12.10ns     424.03ns       299240

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SGb_setsDs        8     227.75ns      32.69ns     notSupport      38.19ns      53.56ns          288
SGb_setsDs       16     414.19ns      31.55ns     notSupport      24.52ns     197.28ns          544
SGb_setsDs       32     352.58ns      32.30ns     notSupport      19.11ns      76.58ns         1056
SGb_setsDs       64     438.94ns      49.36ns     notSupport      17.48ns      62.27ns         2080
SGb_setsDs      128     536.94ns      52.71ns     notSupport      15.80ns      72.06ns         4128
SGb_setsDs      256     599.72ns      57.42ns     notSupport      14.73ns      61.49ns         8224
SGb_setsDs      516     615.28ns      59.69ns     notSupport      14.03ns      58.50ns        16544
SGb_setsDs     1024     686.17ns      59.32ns     notSupport      14.18ns      64.97ns        32800
SGb_setsDs     2048     748.20ns      61.92ns     notSupport      22.53ns      73.59ns        65568
SGb_setsDs     4096     812.11ns      64.99ns     notSupport      13.44ns      70.28ns       131104
SGb_setsDs     8192     872.49ns      69.48ns     notSupport      13.46ns      72.19ns       262176
SGb_setsDs    16384    1005.29ns      72.60ns     notSupport      13.50ns      78.52ns       524320

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SOrddictDs        8     195.45ns      33.52ns        57.58ns      35.48ns      50.46ns          328
SOrddictDs       16      25.74ns      54.86ns       132.97ns      22.51ns     151.75ns          648
SOrddictDs       32      20.61ns      87.80ns       253.75ns      17.38ns     169.30ns         1288
SOrddictDs       64      44.20ns     157.32ns       390.48ns      15.43ns     296.45ns         2568
SOrddictDs      128      72.32ns     318.14ns       578.03ns      13.76ns     489.81ns         5128
SOrddictDs      256      64.55ns     601.77ns       957.25ns      12.81ns     886.69ns        10248
SOrddictDs      516      71.91ns    1108.16ns      1929.03ns      12.22ns    1670.19ns        20648
SOrddictDs     1024      54.42ns    2148.09ns      3541.91ns      11.98ns    3526.90ns        40968
SOrddictDs     2048      46.97ns    4301.59ns      7402.99ns      12.08ns    7580.66ns        81928
SOrddictDs     4096      49.73ns    8577.73ns     15093.63ns      12.19ns   14415.53ns       163848
SOrddictDs     8192      53.15ns   17225.73ns     31139.08ns      12.25ns   29504.86ns       327688
SOrddictDs    16384      45.70ns   34267.85ns     61983.50ns      12.66ns   62566.49ns       655368

DsName        V_Num   insert/per     read/per     update/per      for/per   delete/per     termSize
=====================================================================================
SOrdsetsDs        8      24.14ns      35.44ns     notSupport      31.48ns      42.23ns          136
SOrdsetsDs       16      18.32ns      44.50ns     notSupport      19.19ns     111.36ns          264
SOrdsetsDs       32      14.78ns      66.19ns     notSupport      14.31ns     163.28ns          520
SOrdsetsDs       64      12.97ns     114.61ns     notSupport      10.77ns     301.99ns         1032
SOrdsetsDs      128      52.49ns     255.71ns     notSupport       9.21ns     588.87ns         2056
SOrdsetsDs      256      43.45ns     519.79ns     notSupport       8.13ns     916.57ns         4104
SOrdsetsDs      516      28.83ns     848.03ns     notSupport       7.49ns    1716.34ns         8264
SOrdsetsDs     1024      37.84ns    1876.50ns     notSupport       7.54ns    3306.97ns        16392
SOrdsetsDs     2048      27.21ns    3388.39ns     notSupport       7.28ns    6934.55ns        32776
SOrdsetsDs     4096      25.35ns    7432.09ns     notSupport       7.47ns   14088.49ns        65544
SOrdsetsDs     8192      24.43ns   12985.49ns     notSupport       7.42ns   28212.04ns       131080
SOrdsetsDs    16384      20.96ns   26353.72ns     notSupport       7.43ns   58574.89ns       262152
ok
