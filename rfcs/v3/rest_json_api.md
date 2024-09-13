# REST API Output

Hasura v3 will auto-generate a [JSON:API](https://jsonapi.org/) spec-compliant REST API output.
Hasura v3 will only generate these for DDN models. Commands are out of scope of this RFC.

## Fetching Data

Data, including resources and relationships, can be fetched by sending a GET request to an endpoint.

Responses can be further refined with the optional features described below.

### Fetching Resources

A server MUST support fetching resource data for every URL provided as:

* a self link as part of the top-level links object
* a self link as part of a resource-level links object
* a related link as part of a relationship-level links object

### Selecting

JSON:API supports selecting specific fields of a resource and specific fields of its related
resources.

An endpoint **MAY** also support an `include` query parameter to allow the client to customize which
related resources should be returned.

A client **MAY** request that an endpoint return only specific
[fields](https://jsonapi.org/format/#document-resource-object-fields) in the response on a per-type
basis by including a `fields[TYPE]` query parameter. They are called [sparse
fieldsets](https://jsonapi.org/format/#fetching-sparse-fieldsets).

```
GET /articles/1?include=comments

GET /articles/1?include=comments.author

GET /articles/1?include=comments.author,ratings

GET /articles/1/relationships/comments?include=comments.author

GET /articles?include=author&fields[articles]=title,body&fields[people]=name // sparse fieldsets
```

### Select Many

```
GET /<modelname>

GET /articles
```

#### Filtering

JSON:API is agnostic about the syntax and implementation of the server. The only requirement is to
have a `filter` query parameter.

So we propose the following for complex filtering use-cases.

```
GET /<modelname>?filter[field1]=val1&filter[field2]=val2,val3&filter[field3.reltarget_field1]=val4

GET /comments?filter[post]=1,2&filter[author.id]=12

GET /movies?filter={"$or":[{"name":{"$eq":"Braveheart"}},{"name":{"$eq":"Gladiator"}}]} // url encoded
```

Both the above syntax are equivalent. If you notice, the last syntax is equivalent to what Hasura
generates for its GraphQL API.

Hasura will also generate an OpenAPI schema for complex filtering on nested types. As a result, any
auto-generated client would get full auto-complete and type-checking on any nested filtering
clauses. Check out <https://github.com/hasura/json-api-experiments> to see this in action.

#### Sorting

```
GET /<modelname>?sort=field1,-field2

GET /articles?sort=-created,title
```

The sort order for each sort field MUST be ascending unless it is prefixed with
 a minus (U+002D HYPHEN-MINUS, “-“), in which case it MUST be descending.

#### Pagination

```
GET /<modelname>?page[limit]=10&page[offset]=0

GET /articles?page[limit]=10&page[offset]=0
```

The following keys MUST be used for pagination links:

* first: the first page of data
* last: the last page of data
* prev: the previous page of data
* next: the next page of data

### Select One

```
GET /<modelname>/<unique-identifier>

GET /articles/1
GET /articles/1
GET /articles/1/author
```

## Aggregates

JSON:API doesn’t have support for aggregates. It can be supported via JSON:API extensions. But for
now, it is kept it out of scope.

## Commands

JSON:API doesn't have support for exposing endpoints for a one-off job. It can be supported via
JSON:API extensions. But for now, it is kept it out of scope.

For now, it is kept out of scope to keep the scope small.

### Mutations

JSON:API supports mutating resources via `POST`, `PUT`, `PATCH` and `DELETE` HTTP verbs on the
resource endpoints. This is a different model compared to Hasura, where Hasura follows the CQS
(Command-Query separation) pattern.

Supporting first-class mutation of resources via HTTP verbs would be a departure from the Hasura's
API model. Hence it is kept out of scope of this RFC for now.

## Customizations

If required, response transformation could be performed with a JSON:API output on Hasura.

### Response Transforms Example

#### Flatten Response JSON

Say we want to flatten a field of the JSON response. We want to change `article.author.name` to
`article.autor_name`.

Recommended Approach: Use TS Connector

* Becomes part of relationship response object

```javascript

  "data": {
	"articles": [
  	{
    	"id": 1,
    	"title": "Article Title 1",
    	"author": {
      		"name": "Author Name 1"
    	  }
  	},
  	{
    	"id": 2,
    	"title": "Article Title 2",
    	"author": {
      		"name": "Author Name 2"
    	  }
  	}


[
  { id: 1, title: "Article Title 1", author_name: "Author Name 1" },
  { id: 2, title: "Article Title 2", author_name: "Author Name 2" }
]
```

Typescript Function

```javascript
export function author_name(author?: AuthorType): string {
  if (!author) {
	return "Invalid author";
  }
  return author.name;
}
```

Type to Command Relationship

```
kind: Relationship
version: v1
definition:
  name: author_name
  source: articles
  target:
	command:
  		name: author_name
  		subgraph: null
  mapping:
	- source:
    		fieldPath:
      			- fieldName: author
  	  target:
    		argument:
    		  	argumentName: author
```

In the future when computed columns are supported in OpenDD, it can be part of the general response
type and not relationships

## Response Document Structure

Responses MUST be according to the JSON:API document structure.

* JSON object must be root of every document
* A document MUST contain at least one of the following top-level members:

```
  data: the document’s “primary data”, either a single resource object, an array of resource objects, or null.

  errors: an array of error objects.

  meta: contains non-standard meta-information.
```

* The members \`data\` and \`errors\` MUST NOT coexist in the same document.
* Primary data MUST be
  * single resource object
  * array of resource objects

### Top-Level Structure:

- A JSON document **MUST** contain at least one of the following top-level members:
  - `data`: The document’s “primary data”.
    - is either a single resource object, an array of resource objects, or null.
  - `errors`: An array of error objects.
  - `meta`: A meta object that contains non-standard meta-information.
- Other Top Level Items
  - `included:` Includes related (to the primary data) resource objects and is used to provide
    compound documents. The purpose of this section is to reduce the number of requests clients need
    to make by including related resources directly in the response.
    - Compound Documents \- Allow responses to include related resources along with the primary
      resources.
      - A [compound document](https://jsonapi.org/format/\#document-compound-documents) **MUST NOT**
        include more than one resource object for each `type` and `id` pair.
      - Every included resource object **MUST** be identified via a chain of relationships
        originating in a document’s primary data. This means that compound documents require “full
        linkage” and that no resource object can be included without a direct or indirect
        relationship to the document’s primary data.
    - Can also contain relationships and related links, just like the primary `data` resource
      objects
  - `links`: Top-level links object contains pagination links (`self`, `next`, `last`).
  - `jsonapi:` Contains the version of the JSON
  - being used and additional meta-information about the API, including a `describedby` URL.

Read more on the [resource object](https://jsonapi.org/format/\#document-resource-objects).

# OpenDD config

For the first iteration, to keep things simple, we are not introducing any OpenDD configuration. Any
OpenDD model that is configured with a `graphql` section will get auto-generated JSON:API endpoints.
