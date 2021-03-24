
Updating / installing...
   1:arangodb3e-3.5.3-1.0             ################################# [100%]

ArangoDB 3 (https://www.arangodb.com)
  The multi-model NoSQL database: distributed free and open-source database
  with a flexible data model for documents, graphs, and key-values. Build
  high performance applications using a convenient SQL-like query language
  or JavaScript extensions.

First Steps with ArangoDB:
  https://docs.arangodb.com/latest/Manual/GettingStarted/

Configuring the storage Engine:
  https://docs.arangodb.com/latest/Manual/Administration/Configuration/GeneralArangod.html#storage-engine

Configuration file:
  /etc/arangodb3/arangod.conf

Start ArangoDB shell client:
  > /usr/bin/arangosh

Start ArangoDB service:
  > systemctl start arangodb3.service

Enable ArangoDB service:
  > systemctl enable arangodb3.service

SECURITY HINT:
run 'arango-secure-installation' to set a root password
the current password is '7c8329a66d35a624bcfedfae0c944938'
(You should do this for a FRESH install! For an UPGRADE the password does not need to be changed)
2019-12-13T15:41:20Z [1134] INFO [a1c60] {syscall} file-descriptors (nofiles) hard limit is 65535, soft limit is 65535
2019-12-13T15:41:21Z [1134] INFO [7da27] {startup} server will now shut down due to upgrade, database initialization or admin restoration.
