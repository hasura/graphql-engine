# RFC: NoSQL sampling for automatic schema generation

As we operate in an environment encompassing both SQL and NoSQL databases, the inherent unstructured nature of NoSQL presents specific challenges. 

Despite the flexibility and scalability advantages of NoSQL databases, managing data efficiently becomes a complex task, particularly in the context of ensuring a predictable API and improved coding ergonomics.


## Problems / description
- **Inherent unstructured nature of NoSQL**: NoSQL databases, such as MongoDB, lack a predefined schema. This feature, while enabling flexibility, complicates the definition and management of data.
- **GraphQL provides guardrails, but requires structure**: GraphQL introduces un-opinionated guardrails around the NoSQL data source. Our approach provides type safety, enhances execution performance, enables predictable APIs, and improves coding ergonomics.
- **Ease and speed of onboarding**: The current state renders onboarding with GraphQL and NoSQL databases a complex process. The absence of a predefined schema in NoSQL databases makes it harder for newcomers to "plug-in" their data sources the same way they would with SQL databases.
- **Limited used of validation schema**: One of our current solutions is to use the validation schema in MongoDB. However, this solution is not widely used by our users.
- **Feature parity**: Providing users with capabilities to instantly introspect and track their Collections and Documents across MongoDB databases can expedite their onboarding process with Hasura and elevate NoSQL databases to feature parity with other schema-based databases supported by Hasura.

## Proposed solution
To address these challenges, the proposal is for an automatic schema generation tool leveraging NoSQL sampling techniques is proposed.

A proof-of-concept is below for MongoDB, and the same approach can be applied to other NoSQL databases.

Essentially, the idea around is that we can use sampling to take either the entire, or a subset of a collection's documents. 
Then we would run an analysis to get an idea about the shapes and types that are found within the universe of documents in the schema. Then we would use that analysis to generate a schema which could be used as an onboarding starting point to power the GraphQL schema in Hasura.

This schema would then be customizable to allow for changes as needed by the end-user.

## Open questions
- What kind of selectors are necessary to be able to select the right documents?
  - For example in MongoDB, we have the ability to select documents based on a query, maxed depth, based on a percentage of the collection, or based on a max number of records.
- How should the tooling select which type to use when there are conflicting types?
  - For example, if a certain field has majority `int` types and a couple of `string` types, which should be taken?
    - Would this need to be configurable?
- When authoring the schema, should we have the option
  - For example, certain situations where very few optional embedded-objects which are contained only in a few records in a collection.
  - Should we have something like a 'must have been found in x % of documents' setting?
- Should this be implemented into core tools, or is there a flexibility in being able to have this as part of an external set of tools?
- How can this be implemented in a way which is reproducible for other NoSQL database vendors.

-----

## Proof-of-concept

We've created a proof of concept for a schema sampler that can be run against a MongoDB database.

It's built using a combination of **mongosh** (https://www.mongodb.com/docs/mongodb-shell/), **Variety** MongoDB schema analyzer (https://github.com/variety/variety), and Node.js.

It generates a MongoDB validation schema based on the analysis of the documents in the collection, and then optionally will apply it to the database. 

*[Later this same work can be used for generating Hasura-representations of the schema using our logical models as well.]*

That schema can then be used by Hasura to generate a GraphQL schema on top of the MongoDB datasource.

### How do I get started
- `docker compose up`
  - Runs MongoDB and loads up the `sample_mflix` sample database.
  - Has a healthcheck so the sampler shouldn't run till the database is ready.
  - Sampler runs `/schema_sampler/archive.sh` which bootstraps introspecting the collections, running them through variety, converting the analysis to a validation schema, and then updating that schema back into MongoDB.

### How can I customize what is being introspected and sampled?
The docker container currently contains some environment variables below which can be used for selecting the collections to analyze and sample.

If you'd like to customize the sampling method, you can edit the `./schema_sampler/analyze.sh` file.

On line 29 there's:
```bash
mongosh ${MONGO_DATABASE} --quiet --eval "var collection = '${collection//\'/}', outputFormat='json'" --username ${MONGO_USERNAME} --password ${MONGO_PASSWORD} --authenticationDatabase=admin /schema_sampler/variety.js > "/schema_exports/analysis/${collection//\'/}.json"
```
which is where the data is retrieved for sampling using `mongosh`. You can edit the `--eval` command to change the sampling method (for example, adding a `find()` to only records using version of a schema, or a `limit()` to only return the first 5000 records).


### Want to know more about what's happening
Running `docker logs mongodb_sampling` would allow you to see what was run when inside the sampling container.

### ./schema_exports volume mount
Will contain the intermediate analysis files and JSON files for the validation schema export.

### Docker environment variables
The `mongodb_sampler` container has a few environment variable helpers which you can set:
- `MONGO_DATABASE`: The MongoDB connection string.
- `MONGO_USERNAME`: The MongoDB username.
- `MONGO_PASSWORD`: The MongoDB password.
- `MONGO_SELECT_COLLECTIONS`: Which collections to analyze and sample. ('' [for all collections]), (movies,comments)
- `MONGO_UPDATE_COLLECTIONS`: Automatically update collections in the database with the generated validation schemas. (true or false [or blank for false])