# Cassandra

1. RDBMS with extremely high write throughput
2. Query first data modeling - with denormalized and duplicate data to optimize for reads
3. Data for particular partition is always stored within a partition
   - All reads and writes must have the partition key

Hasura + Cassandra

1. Parameterized models for reads to ensure partition key is always given
2. Simple writes, but writes are probably done by a backend system
