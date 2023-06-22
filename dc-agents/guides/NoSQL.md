# Hasura + NoSQL (JSON)

Mongo is awesome because:

1. It is super flexible and developers decide what to do as their requirements change. Eg: All types of read models with varying degrees of normalization. Writes can have different types of guarantees of consistency that allow developers to control how fast or slow writes are.
2. Reads & writes are super fast, easy to vertically and horizontally scale, easy to shard and geo-distribute.

What Hasura needs to make Hasura + Mongo awesome:

1. Support modeling and querying nested (embedded) documents well since that is the dominant pattern
2. Support relationships on complex LHS arguments (nested objects/arrays) with native & awesome mongo ways:
   - `$lookup`, `$unwind`: similar to joins
   - `$graphLookup`: recursive join! great if we can bring that value to the fore :)
3. Support aggregation pipelines in parameterized models
4. Other R&D ideas that need significant guidance from mongo users/customers:
   - Help data/schema migration on mongo
   - Support denormalized writes?
