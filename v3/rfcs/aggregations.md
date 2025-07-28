# Aggregates and Groups RFC

A demo GraphQL API can be played with that demonstrates the intended GraphQL API
shape, but does not return any results:
https://cloud.hasura.io/public/graphiql?endpoint=https%3A%2F%2Fhasura-v3-aggregates-mock.deno.dev
This API is built using the GraphQL types defined later in this RFC. The code
for it is here: https://github.com/hasura/v3-aggregates-mock

## v3 Goals

- Flip aggregates in the GraphQL schema from `max { column }` to
  `column { max }`
- Remove support for multiple column count
- Special case COUNT(\*) as `_count`
- Try to make as type-safe a GraphQL API as possible; minimise users being able
  to write queries that fail to execute
- Rework aggregation predicates so that they can work over more than just
  float-typed fields (weakness of v2 implementation)
- Support aggregate functions that take multiple parameters. The extra
  parameters cannot be columns.
- Add support for group by-style aggregation queries

## Implementation Milestones

These are milestones that we definitely should implement:

- `_aggregate` root field
- `_aggregate` array relationship fields
- Ordering by aggregations
- Filtering by aggregations (aggregation predicates)
- `_aggregate` for nested arrays
- `_groups` root field
- `_groups` array relationship fields

These are milestones that we may wait for user feedback or performance testing
to see whether we really want to implement them:

- Grouping over aggregations
- n-ary aggregate function support
- Argument presets for n-ary aggregation functions
- Grouping types (Rollup/Cube/etc)

The following are features illustrated in this RFC that are not actually in the
RFC (because all these changes need to compose together correctly) and will be
implemented separately:

- Filtering by nested fields
- Ordering by nested fields
- `BooleanExpressionType` Open DD kind
- `OrderByExpression` Open DD kind

## Data Model

This data model is a variant on some tables from Chinook, modified slightly to
add some nested type features. This data model was chosen to include the
following features:

- Object relationships
- Array relationships
- Nested objects
- Nested arrays of scalars
- Nested arrays of objects

```typescript
// Table
type Invoice = {
  InvoiceId: int; // PK
  InvoiceDate: date;
  CustomerId: int; // FK: Customer table
  Discounts: Discount[]; // Nested array of objects
  Total: decimal;
  BillingAddress: Address; // Nested object
};

// Nested Object Type
type Discount = {
  Description: string;
  Percentage: decimal;
};

// Nested Object Type
type Address = {
  StreetAddress: string;
  City: string;
  State: string;
  PostalCode: string;
  Country: string;
};

// Table
type Customer = {
  CustomerId: int; // PK
  FirstName: string;
  LastName: string;
  Address: Address; // Nested object
  MobilePhone: string;
  Emails: string[]; // Nested array of scalars
  SupportRepId: int | null;
};

// Table
type InvoiceLine = {
  InvoiceLineId: int; // PK
  InvoiceId: int; // FK: Invoice table
  TrackId: int;
  Quantity: int;
  UnitPrice: Multicurrency; // Nested object
};

// Nested Object Type
type Multicurrency = {
  Currency: string; //eg. AUD, USD, etc
  Value: decimal;
};
```

## Aggregate Functions

Some functions have been chosen as a representative sample of the different
sorts of aggregation functions found in the wild. They are categorised into
three categories.

### Unary Functions

Unary functions are aggregate functions that simply operate on a single column
(ie. they take one argument, the column). Examples:

- Numeric-type functions
  - `SUM(column)`
  - `AVG(column)`
- Orderable-type functions
  - `MIN(column)`
  - `MAX(column)`

### n-ary Functions

n-ary functions are aggregate functions that operate over a single column, but
also take additional parameters to configure the aggregation. Examples:

- `CONCAT(column, separator)` - Concatenates strings together, separating then
  with the `separator` string.

### Special Case: COUNT

`COUNT(*)` is a special case of an aggregate function that does not take any
columns and simply counts the number of rows in the result set. `COUNT(column)`
is another variant that counts all rows with a non-null value for column, and
`COUNT(DISTINCT column)` is the same but it does not count duplicated values.

## Aggregations Walkthrough

This section walks through examples of aggregation queries and how parts of them
can be used.

### Get aggregates of scalar fields

In this example, we can aggregate over multiple fields in the Invoice
collection, as well as counting the number of objects in the collection.

```graphql
query MyQuery {
  Invoice_aggregate {
    InvoiceDate {
      _max # The most recent invoice date
    }
    Total {
      _max # The biggest total on an invoice
      _sum # The total spent across all invoices
    }
    _count # The number of invoices
  }
}
```

### Get aggregates of scalar fields, and also return the objects

If we want to also return the objects, we'll need to use a separate root field.

```graphql
query MyQuery {
  Invoice {
    InvoiceId
    Total
  }
  Invoice_aggregate {
    InvoiceDate {
      _max # The most recent invoice date
    }
    Total {
      _max # The biggest total on an invoice
      _sum # The total spent across all invoices
    }
    _count # The number of invoices
  }
}
```

### Perform aggregation into nested objects

We can also perform aggregations over nested object fields.

```graphql
query MyQuery {
  Invoice_aggregate {
    BillingAddress {
      _count # Total number of non-null BillingAddresses across all Invoices
      PostalCode {
        _min # Smallest postal code
      }
    }
  }
}
```

### Perform filtering, ordering and pagination of objects

We can filter, order and paginate the objects before aggregating them using
`filter_input`.

```graphql
query MyQuery {
  Invoice_aggregate(
    filter_input: {
      where: { Total: { _gt: 100 } } # Only include invoices with over $100 spend
      order_by: [{ InvoiceDate: Desc }, { Customer: { LastName: Asc } }] # Order by InvoiceDate desc, then Customer's Last Name asc
      offset: 10 # Skip the first 10 results
      limit: 10 # Only return the first 10 results after skipping
    }
  ) {
    Total {
      _max # The highest invoice total within the selected objects
    }
  }
}
```

### Aggregate nested arrays of objects

We can perform aggregations over nested arrays of objects.

```graphql
query MyQuery {
  Invoice {
    # All invoices
    Discounts_aggregate {
      # Aggregate each Invoice's Discounts nested array
      Percentage {
        _max # The highest percentage discount in each invoice
      }
    }
  }
}
```

### Aggregate nested arrays of scalars

We can also perform aggregations over a nested array of scalars.

```graphql
query MyQuery {
  Customer {
    # Select all Customer objects
    Emails_aggregate {
      # Aggregate each customer's emails array
      _count # The total number of non-null entries in the array
      _max # The alphabetically last email in the array
    }
  }
}
```

### Aggregate using aggregation functions that take parameters

We can aggregate using aggregation functions that take extra parameters.

```graphql
query MyQuery {
  Customer {
    Emails_aggregate {
      _concat(separator: ",") # Concatenate all the Invoice's customer's emails together separated by a comma
    }
  }
}
```

### Perform aggregations across relationships

We can perform aggregations of array-related collections.

```graphql
query MyQuery {
  Invoice {
    # All invoices
    InvoiceLines_aggregate {
      # Aggregate each invoice's related InvoiceLines
      _count # Number of invoice lines in each Invoice
    }
  }
}
```

### Order by the results of aggregations

We can order collections by the results of performing an aggregation.

```graphql
query MyQuery {
  Invoice(
    order_by: [{ InvoiceLines_aggregate: { _count: Desc } }] # Order by the number of invoice lines in each invoice, descending
  ) {
    InvoiceId
    InvoiceLines_aggregate {
      _count
    }
  }
}
```

### Order by the results of an aggregation function that takes extra arguments

If we have an n-ary aggregate function, we can still order by it, but the syntax
gets a bit more complicated.

```graphql
query MyQuery {
  Invoice(
    # Order Invoices by Invoice Customer's Emails once they have been concated together with a comma, ascending
    order_by: [
      {
        Customer: {
          Emails_aggregate: {
            _concat: { args: { separator: "," }, ordering: Asc }
          }
        }
      }
    ]
  ) {
    InvoiceId
    Customer {
      Emails_aggregate {
        _concat(separator: ",")
      }
    }
  }
}
```

### Filter by the results of an aggregation (aggregation predicates)

We can filter our collections by the results of applying aggregations to array
relationships (and nested arrays).

```graphql
query MyQuery {
  Invoice(
    where: {
      _and: [
        {
          # Filter by invoices that have at least one invoice line that is priced in the AUD currency ...
          InvoiceLines_aggregate: {
            filter_input: { where: { UnitPrice: { Currency: { _eq: "AUD" } } } } # Filters the collection before aggregating (optional)
            predicate: { _count: { _gt: 0 } } # The predicate over the results of aggregations
          }
        }
        {
          # ... and that the average quantity of across invoice lines is greater than 5
          InvoiceLines_aggregate: {
            predicate: { Quantity: { _avg: { _gt: 5 } } }
          }
        }
      ]
    }
  ) {
    InvoiceId
  }
}
```

## Group By Walkthrough

This section walks through the new `groups` queries and how each new part of
them can be used.

### Group by a scalar field, order the groups and aggregate within the groups

In this example, we group by a single field and aggregate over the group. We
order the groups by the key of the group, ascending.

```graphql
query MyQuery {
  Invoice_groups(
    grouping_keys: [{ BillingAddress: { _scalar_field: Country } }] # Group by BillingAddress.Country
    order_by: [{ group_key: { BillingAddress: { Country: Asc } } }] # Sort groups by BillingAddress.Country asc
  ) {
    group_key {
      BillingAddress {
        Country # The value of the billing address country per group
      }
    }
    group_aggregate {
      _count # The number of invoices per BillingAddress.Country
      BillingAddress {
        State {
          _min # Smallest BillingState per BillingCountry lexicographically
        }
      }
    }
  }
}
```

### Group by multiple scalar field

We can continue grouping by multiple scalar fields by adding more fields. We can
also order our groups by the results of a grouping aggregate.

```graphql
query MyQuery {
  Invoice_groups(
    # Group by BillingAddress.Country, then BillingAddress.State
    grouping_keys: [
      { BillingAddress: { _scalar_field: Country } }
      { BillingAddress: { _scalar_field: State } }
    ]
    # Sort groups first by BillingAddress.Country asc, then by the number of states per country descending, then by BillingState descending
    order_by: [
      { group_key: { BillingAddress: { Country: Asc } } }
      { group_aggregate: { _count: Desc } }
      { group_key: { BillingAddress: { State: Desc } } }
    ]
  ) {
    group_key {
      # The value of the (billing country, billing state) tuple per group
      BillingAddress {
        Country
        State
      }
    }
    group_aggregate {
      _count # The number of invoices per BillingAddress.State per BillingAddress.Country
    }
  }
}
```

### Rollup aggregations

If we want to perform the same set of aggregations at each level of a grouping,
we can perform a rollup aggregation, which will perform the aggregation across
all rows, across every billing country, and across every billing country/billing
state pair.

To represent the rolled-up groups, null is substituted for each of the group
keys in the results. So we have the following groups:

- `BillingCountry, BillingState` (all invoices in a state in a country)
- `BillingCountry, null` (all invoices in the specified country)
- `null, null` (all invoices)

Note: This gets a bit weird where one of the group keys can be null; you will
end up with the rollup row with the same group key as an actual group. This
seems to be how SQL does it. 😢

```graphql
query MyQuery {
  Invoice_groups(
    # Group by BillingAddress.Country, then BillingAddress.State
    grouping_keys: [
      { BillingAddress: { _scalar_field: Country } }
      { BillingAddress: { _scalar_field: State } }
    ]
    # Perform grouping using the rollup strategy
    grouping_type: Rollup
    # Sort groups first by BillingAddress.Country asc, then by BillingAddress.State descending
    order_by: [
      { group_key: { BillingAddress: { Country: Asc } } }
      { group_key: { BillingAddress: { State: Desc } } }
    ]
  ) {
    group_key {
      # The value of the (billing country, billing state) tuple per group.
      BillingAddress {
        Country
        State
      }
    }
    group_aggregate {
      _count # The number of invoices in a group
    }
  }
}
```

### Cube aggregations

Cube aggregations are like rollup aggregations, except that the data is grouped
for all possible combinations of fields.

So for the below example, the groups would be:

- `BillingCountry, BillingState` (all invoices in a state in a country)
- `BillingCountry, null` (all invoices in the specified country)
- `null, BillingState` (all invoices in the specified state, grouping together
  states with the same name across countries)
- `null, null` (all invoices)

Note: This gets a bit weird where one of the group keys can be null; you will
end up with the rollup row with the same group key as an actual group. This
seems to be how SQL does it. 😢

```graphql
query MyQuery {
  Invoice_groups(
    # Group by BillingAddress.Country, then BillingAddress.State
    grouping_keys: [
      { BillingAddress: { _scalar_field: Country } }
      { BillingAddress: { _scalar_field: State } }
    ]
    # Perform grouping using the rollup strategy
    grouping_type: Cube
    # Sort groups first by BillingAddress.Country asc, then by BillingAddress.State descending
    order_by: [
      { group_key: { BillingAddress: { Country: Asc } } }
      { group_key: { BillingAddress: { State: Desc } } }
    ]
  ) {
    group_key {
      # The value of the (billing country, billing state) tuple per group.
      BillingAddress {
        Country
        State
      }
    }
    group_aggregate {
      _count # The number of invoices in a group
    }
  }
}
```

### Apply aggregations at multiple levels of the grouping

One can't apply arbitrary aggregations at each level of the grouping, but one
can simply repeat the grouping to achieve the same result:

```graphql
query MyQuery {
  # Group by BillingAddress.Country
  BillingCountryGrouping: Invoice_groups(
    grouping_keys: [{ BillingAddress: { _scalar_field: Country } }]
    order_by: [{ group_key: { BillingAddress: { Country: Asc } } }]
  ) {
    group_key {
      BillingAddress {
        Country # The value of BillingAddress.Country for each group
      }
    }
    group_aggregate {
      _count # Number of invoices per BillingCountry
    }
  }
  # Group by BillingCountry then by BillingState, order groups by BillingAddress.Country descending, then by BillingAddress.State desc
  CountryThenStateGroup: Invoice_groups(
    grouping_keys: [
      { BillingAddress: { _scalar_field: Country } }
      { BillingAddress: { _scalar_field: State } }
    ]
    order_by: [
      { group_key: { BillingAddress: { Country: Asc } } }
      { group_key: { BillingAddress: { State: Desc } } }
    ]
  ) {
    group_key {
      BillingAddress {
        Country # The value of BillingAddress.Country for each group
        State # The value of BillingAddress.State for each group
      }
    }
    group_aggregate {
      _count # Number of invoices per date
    }
  }
}
```

### Filtering, ordering and paginating the objects grouped over

We can filter, order and paginate the objects we group over by applying a
`filter_input` at the root field level.

```graphql
query MyQuery {
  Invoice_groups(
    filter_input: {
      where: { BillingAddress: { Country: { _eq: "Australia" } } } # Filter by BillingAddress.Country
      order_by: { Total: Desc } # Order by highest total invoices first
      limit: 100 # Include the first 100 only
    }
    grouping_keys: [{ BillingAddress: { _scalar_field: State } }] # Then, group by BillingAddress.State
  ) {
    group_key {
      BillingAddress {
        State # The value of BillingAddress.State for each group
      }
    }
    group_aggregate {
      _count
    }
  }
}
```

### Filtering the groups by group-aggregate results

If we want to filter the groups by the aggregations over the groups themselves,
we can use `having` on the root field.

```graphql
query MyQuery {
  Invoice_groups(
    grouping_keys: [{ _scalar_field: InvoiceDate }] # Grouping by InvoiceDate
    having: { _count: { _gt: 1 } } # Filter the groupings where the number of invoices in the group is > 1
  ) {
    group_key {
      InvoiceDate # InvoiceDate for each group
    }
    group_aggregate {
      _count # Number of invoices per date
    }
  }
}
```

### Grouping by fields in an object-related model

We can group by fields in an object-related model if we wish:

```graphql
query MyQuery {
  Invoice_groups(
    grouping_keys: [{ Customer: { _scalar_field: LastName } }] # Group by the invoice's customer's last name
    order_by: [{ group_key: { Customer: { LastName: Asc } } }] # sort groups by last name ascending
  ) {
    group_key {
      Customer {
        # Object relationship navigation
        LastName # The value of the Customer's last name for each group
      }
    }
    group_aggregate {
      Total {
        _sum
      } # Sum of the totals from all invoices with customers that have the same last name
    }
  }
}
```

### Grouping by an aggregation of an array relationship

If we want to group by an array-related model, we will need to group over an
aggregation of that model:

```graphql
query MyQuery {
  Invoice_groups(
    grouping_keys: [
      { InvoiceLines_aggregate: { Quantity: { _unary_fn: _sum } } }
    ] # Group by the sum of the invoice's lines' quantity field...
    order_by: [
      { group_key: { InvoiceLines_aggregate: { Quantity: { _sum: Asc } } } }
    ] # ... and order the groups by it
  ) {
    group_key {
      InvoiceLines_aggregate {
        # Array relationship navigation
        Quantity {
          _sum
        } # The number of items purchased in the invoice (sum of quantities of all lines per invoice)
      }
    }
    group_aggregate {
      _count # Number of invoices that have a certain quantity of items purchased
    }
  }
}
```

## GraphQL Schema Types

This sketches out the GraphQL types necessary to define an aggregations query
field for the Invoice model. The
[proposed GraphQL `@oneof` directive](https://github.com/graphql/graphql-spec/pull/825)
has been used to indicate input union types.

### Collection Selector Types

Type categories:

- `query_root` - The query root type
  - Configurable in OpenDD in
    `GraphqlConfig.definition.query.rootOperationTypeName`
- `<object type>` - Row selection set for the object type
  - Example type: `Invoice`
  - Usage: `{ InvoiceId }`
  - Configurable in OpenDD using `Model` and `ObjectType`
- `<object type>_filter_input` - Filter the input objects that go into an
  aggregation or grouping operation
  - Example type: `Invoice_filter_input`
  - Usage:
    `{ where: { InvoiceId: { _gt: 1 } }, order_by: { InvoiceId: Asc }, offset: 10, limit: 10 }`
  - Configurable in OpenDD:
    - Name: `Model.definition.graphql.filterInputTypeName`
    - Input args are configured via `GraphqlConfig.definition.query` settings
      such as `filterInput`, `limitInput` etc
- `Group_by_grouping_type` - Select a grouping type to use
  - Usage: `Cube`
  - Configurable in OpenDD: `GraphqlConfig.definition.query.groupInputs`

```graphql
# Root field type
type Query {
  Customer(
    limit: Int
    offset: Int
    order_by: [Customer_order_by!]
    where: Customer_bool_exp
  ): Customer!
  # Configurable in OpenDD Model.definition.graphql.aggregate.queryRootField
  Customer_aggregate(
    # Configurable in OpenDD GraphqlConfig.definition.query.aggregate
    filter_input: Customer_filter_input
  ): Customer_aggregate_fields!
  # Configurable in OpenDD Model.definition.graphql.groups.queryRootField
  Customer_groups(
    # Configurable in OpenDD GraphqlConfig.definition.query.groups
    filter_input: Customer_filter_input
    grouping_keys: [Customer_grouping_key!]!
    grouping_type: Group_by_grouping_type # Omitting this defaults to Standard
    having: Customer_aggregate_bool_exp
    order_by: [Customer_grouping_order_by!]
    offset: Int
    limit: Int
  ): [Customer_groups!]!

  Invoice(
    limit: Int
    offset: Int
    order_by: [Invoice_order_by!]
    where: Invoice_bool_exp
  ): Invoice!
  # Configurable in OpenDD Model.definition.graphql.aggregate.queryRootField
  Invoice_aggregate(
    # Configurable in OpenDD GraphqlConfig.definition.query.aggregate
    filter_input: Invoice_filter_input
  ): Invoice_aggregate_fields!
  # Configurable in OpenDD Model.definition.graphql.groups.queryRootField
  Invoice_groups(
    # Configurable in OpenDD GraphqlConfig.definition.query.groups
    filter_input: Invoice_filter_input
    grouping_keys: [Invoice_grouping_key!]!
    grouping_type: Group_by_grouping_type # Omitting this defaults to Standard
    having: Invoice_aggregate_bool_exp
    order_by: [Invoice_grouping_order_by!]
    offset: Int
    limit: Int
  ): [Invoice_groups!]!

  InvoiceLine(
    limit: Int
    offset: Int
    order_by: [InvoiceLine_order_by!]
    where: [InvoiceLine_bool_exp!]
  ): [InvoiceLine!]!
  # Configurable in OpenDD Model.definition.graphql.aggregate.queryRootField
  InvoiceLine_aggregate(
    # Configurable in OpenDD GraphqlConfig.definition.query.aggregate
    filter_input: InvoiceLine_filter_input
  ): InvoiceLine_aggregate_fields!
  # Configurable in OpenDD Model.definition.graphql.groups.queryRootField
  InvoiceLine_groups(
    # Configurable in OpenDD GraphqlConfig.definition.query.groups
    filter_input: InvoiceLine_filter_input
    grouping_keys: [InvoiceLine_grouping_key!]!
    grouping_type: Group_by_grouping_type # Omitting this defaults to Standard
    having: InvoiceLine_aggregate_bool_exp
    order_by: [InvoiceLine_grouping_order_by!]
    offset: Int
    limit: Int
  ): [InvoiceLine_groups!]!
}

type Invoice {
  # Scalar fields
  InvoiceId: Int!
  InvoiceDate: Date!
  CustomerId: Int!
  Total: Decimal!

  # Nested object fields/object relationships
  BillingAddress: Address!
  Customer: Customer!

  # Array relationships/nested array of objects
  InvoiceLines(
    limit: Int
    offset: Int
    order_by: [InvoiceLine_order_by!]
    where: [InvoiceLine_bool_exp!]
  ): [InvoiceLine!]!
  # Configurable in OpenDD Relationship.definition.graphql.aggregateFieldName
  InvoiceLines_aggregate(
    # Configurable in OpenDD GraphqlConfig.definition.query.aggregate
    filter_input: InvoiceLine_filter_input
  ): InvoiceLine_aggregate_fields!
  # Configurable in OpenDD Relationship.definition.graphql.groupsFieldName
  InvoiceLines_groups(
    # Configurable in OpenDD GraphqlConfig.definition.query.groups
    filter_input: InvoiceLine_filter_input
    grouping_keys: [InvoiceLine_grouping_key!]!
    grouping_type: Group_by_grouping_type # Omitting this defaults to Standard
    having: InvoiceLine_aggregate_bool_exp
    order_by: [InvoiceLine_grouping_order_by!]
    offset: Int
    limit: Int
  ): [InvoiceLine_groups!]!

  Discounts: [Discount!]!
  Discounts_aggregate: Discount_aggregate_fields!
}

type Address {
  # Scalar fields
  StreetAddress: String!
  City: String!
  State: String!
  PostalCode: String!
  Country: String!
}

type Customer {
  # Scalar fields
  CustomerId: Int!
  FirstName: String!
  LastName: String!
  Address: Address!
  MobilePhone: String!
  SupportRepId: Int

  # Array relationships
  Invoices(
    limit: Int
    offset: Int
    order_by: [Invoice_order_by!]
    where: [Invoice_bool_exp!]
  ): [Invoice!]!
  # Configurable in OpenDD Relationship.definition.graphql.aggregateFieldName
  Invoices_aggregate(
    # Configurable in OpenDD GraphqlConfig.definition.query.aggregate
    filter_input: Invoice_filter_input
  ): Invoice_aggregate_fields!
  # Configurable in OpenDD Relationship.definition.graphql.groupsFieldName
  Invoices_groups(
    # Configurable in OpenDD GraphqlConfig.definition.query.groups
    filter_input: Invoice_filter_input
    grouping_keys: [Invoice_grouping_key!]!
    grouping_type: Group_by_grouping_type # Omitting this defaults to Standard
    having: Invoice_aggregate_bool_exp
    order_by: [Invoice_grouping_order_by!]
    offset: Int
    limit: Int
  ): [Invoice_groups!]!

  # Array of scalars
  Emails: [String!]!
  # Configurable in OpenDD ObjectType.definition.fields[].aggregateExpression & .graphql.aggregateFieldName
  Emails_aggregate: String_aggregate_fields!
}

type InvoiceLine {
  # Scalar fields
  InvoiceLineId: Int!
  InvoiceId: Int!
  TrackId: Int!
  Quantity: Int!

  # Nested object fields/Object relationships
  UnitPrice: Multicurrency!
  Invoice: Invoice!
}

type Multicurrency {
  # Scalar fields
  Currency: String!
  Value: Decimal!
}

type Discount {
  Description: String
  Percentage: Decimal
}

# Configurable in OpenDD GraphqlConfig.definition.groupingType
enum Group_by_grouping_type {
  Standard # Normal SQL GROUP BY
  Rollup # SQL GROUP BY ROLLUP
  Cube # SQL GROUP BY CUBE
}

input Customer_filter_input {
  # Names configurable in OpenDD GraphqlConfig.definition.query.aggregate
  where: Customer_bool_exp
  order_by: [Customer_order_by!]
  offset: Int
  limit: Int
}

input Invoice_filter_input {
  # Names configurable in OpenDD GraphqlConfig.definition.query.aggregate
  where: Invoice_bool_exp
  order_by: [Invoice_order_by!]
  offset: Int
  limit: Int
}

input InvoiceLine_filter_input {
  # Names configurable in OpenDD GraphqlConfig.definition.query.aggregate
  where: InvoiceLine_bool_exp
  order_by: [InvoiceLine_order_by!]
  offset: Int
  limit: Int
}

input Discount_filter_input {
  # Names configurable in OpenDD GraphqlConfig.definition.query.aggregate
  where: Discount_bool_exp
  order_by: [Discount_order_by!]
  offset: Int
  limit: Int
}

input String_filter_input {
  # Names configurable in OpenDD GraphqlConfig.definition.query.aggregate
  where: String_bool_exp
  order_by: order_by
  offset: Int
  limit: Int
}
```

### Order By Input Types

Type categories:

- `<object type>_order_by` - Select an ordering based on something in the object
  type
  - Example type: `Invoice_order_by`
  - Usage: `{ InvoiceId: Asc }`
  - Configurable in OpenDD:
    - Name: `OrderByExpression.definition.graphql.expressionTypeName`

- `<object_type>_aggregate_order_by` - Select an ordering based on an aggregate
  of something about object type
  - Example type: `InvoiceLine_aggregate_order_by`
  - Usage: `{ Quantity: { _sum: Asc } }`
  - Configurable in OpenDD:
    - Name: `AggregateExpression.definition.graphql.orderByInputTypeName`
      (object variant)

- `<scalar type>_aggregate_order_by` - Select an ordering based on an
  aggregation function over something of the scalar type
  - Example type: `String_aggregate_order_by`
  - Usage: `{ _max: Asc }`
  - Configurable in OpenDD:
    - Name: `AggregateExpression.definition.graphql.orderByInputTypeName`
      (object variant)

- `<scalar type>_<n-ary aggregate function>_aggregate_order_by` - Set the
  arguments to pass to the n-ary aggregate function and select an ordering
  - Example type: `String_concat_aggregate_order_by`
  - Usage: `{ args: { separator: ", " }, ordering: Asc }`
  - Configurable in OpenDD:
    - Name:
      `AggregationExpression.definition.operand.scalar.aggregationFunctions[].graphql.orderByArgsInputTypeName`
      (object variant)

- `<scalar type>_<n-ary aggregate function>_aggregate_args` - The arguments to
  pass to the n-ary aggregate function
  - Example type: `String_concat_aggregate_args`
  - Usage: `{ separator: ", " }`
  - Configurable in OpenDD:
    - Name:
      `AggregateExpression.definition.operand.scalar.aggregationFunction[].graphql.argsInputTypeName`
    - Arguments:
      `AggregateExpression.definition.operand.scalar.aggregationFunctions[].arguments`

- `order_by` - Order by direction enum
  - Usage: `Asc`
  - Configurable in OpenDD in `GraphqlConfig.definition.query.orderByInput`

```graphql
# Existing order by type
input Invoice_order_by @oneOf {
  # Scalar fields
  InvoiceId: order_by
  InvoiceDate: order_by
  CustomerId: order_by
  Total: order_by

  # Object relationships & Nested object fields
  Customer: Customer_order_by # Order by type for Customer model
  BillingAddress: Address_order_by

  # Array relationships & nested arrays of objects
  # Configurable in OpenDD Relationship.definition.graphql.aggregateFieldName
  InvoiceLines_aggregate: InvoiceLine_aggregate_order_by # Order by aggregate type for InvoiceLine model
  # Configurable in OpenDD ObjectType.definition.fields[].graphql.aggregateFieldName
  Discount_aggregate: Discount_aggregate_order_by
}

enum order_by {
  Asc
  Desc
}

input Customer_order_by @oneOf {
  # Scalar fields
  CustomerId: order_by
  FirstName: order_by
  LastName: order_by
  MobilePhone: order_by
  SupportRepId: order_by

  # Object relationships & Nested object fields
  Address: Address_order_by

  # Nested array of scalars
  # Configurable in OpenDD ObjectType.definition.fields[].graphql.aggregateFieldName
  Emails_aggregate: String_aggregate_order_by
}

input Address_order_by @oneOf {
  # Scalar fields
  StreetAddress: order_by
  City: order_by
  State: order_by
  PostalCode: order_by
  Country: order_by
}

input Discount_order_by @oneOf {
  # Scalar fields
  Description: order_by
  Percentage: order_by
}

input String_aggregate_order_by @oneOf {
  _max: order_by
  _min: order_by
  _count: order_by
  _count_distinct: order_by
  _concat: String_concat_aggregate_order_by
}

input String_concat_aggregate_order_by {
  args: String_concat_aggregate_args!
  ordering: order_by!
}

input String_concat_aggregate_args {
  separator: String!
}

input InvoiceLine_aggregate_order_by @oneOf {
  _count: order_by # WARN: Potential name clash here
  # Scalar fields
  InvoiceLineId: Int_aggregate_order_by
  InvoiceId: Int_aggregate_order_by
  TrackId: Int_aggregate_order_by
  Quantity: Int_aggregate_order_by

  # Nested object fields
  UnitPrice: Multicurrency_aggregate_order_by
}

input Int_aggregate_order_by @oneOf {
  _avg: order_by
  _sum: order_by
  _max: order_by
  _min: order_by
  _count: order_by
  _count_distinct: order_by
}

input Multicurrency_aggregate_order_by @oneOf {
  _count: order_by # WARN: Potential name clash here
  Currency: String_aggregate_order_by
  Value: Decimal_aggregate_order_by
}

input Decimal_aggregate_order_by @oneOf {
  _avg: order_by
  _sum: order_by
  _max: order_by
  _min: order_by
  _count: order_by
  _count_distinct: order_by
}

input Discount_aggregate_order_by @oneOf {
  _count: order_by # WARN: Potential name clash here
  # Scalar fields
  Description: String_aggregate_order_by
  Percentage: Decimal_aggregate_order_by
}

input InvoiceLine_order_by @oneOf {
  # Scalar fields
  InvoiceLineId: order_by
  InvoiceId: order_by
  TrackId: order_by
  Quantity: order_by

  # Object relationships & Nested object fields
  Invoice: Invoice_order_by # Order by type for Customer model
  UnitPrice: Multicurrency_order_by
}

input Multicurrency_order_by @oneOf {
  # Scalar fields
  Currency: order_by
  Value: order_by
}

input Invoice_aggregate_order_by @oneOf {
  _count: order_by # WARN: Potential name clash here
  # Scalar fields
  InvoiceId: Int_aggregate_order_by
  InvoiceDate: Date_aggregate_order_by
  CustomerId: Int_aggregate_order_by
  Total: Decimal_aggregate_order_by

  # Nested object fields/object relationships
  Address: Address_aggregate_order_by
  Customer: Customer_aggregate_order_by
}

input Date_aggregate_order_by @oneOf {
  _max: order_by
  _min: order_by
  _count: order_by
  _count_distinct: order_by
}

input Address_aggregate_order_by @oneOf {
  # Scalar fields
  StreetAddress: String_aggregate_order_by
  City: String_aggregate_order_by
  State: String_aggregate_order_by
  PostalCode: String_aggregate_order_by
  Country: String_aggregate_order_by
}

input Customer_aggregate_order_by @oneOf {
  # Scalar fields
  CustomerId: Int_aggregate_order_by
  FirstName: String_aggregate_order_by
  LastName: String_aggregate_order_by
  MobilePhone: String_aggregate_order_by
  SupportRepId: Int_aggregate_order_by

  # Object relationships & Nested object fields
  Address: Address_aggregate_order_by
}
```

### Predicate Input Types (including aggregation predicates)

Type categories:

- `<object type>_bool_exp` - Allows comparison against properties of an object
  type, plus boolean logic operators
  - Example type: `Invoice_bool_exp`
  - Usage: `{ InvoiceId: { _eq: 1 } }`
  - Configurable in OpenDD via `OrderByExpression` (object variant)

- `<scalar type>_bool_exp` - Application of comparison functions for the scalar
  type, plus boolean logic operators
  - Example type: `Int_bool_exp`
  - Usage: `{ _eq: 1 }`
  - Configurable in OpenDD via `OrderByExpression` (scalar variant)

- `<object type>_aggregate_predicate_exp` - Top level aggregation predicate for
  the object type, allows setting a filter applied before aggregation, then the
  predicate to evaluate after aggregation
  - Example type: `InvoiceLine_aggregate_predicate_exp`
  - Usage:
    `{ filter_input: { where: { InvoiceId: { _gt: 1 } } }, predicate: { Quantity: { _sum: { _gt: 2 } } } }`
  - Configurable in OpenDD via
    `AggregateExpression.definition.graphql.aggregatePredicateInputTypeName`
    (object variant)

- `<scalar type>_array_aggregate_predicate_exp` - Top level aggregation
  predicate for nested arrays of a scalar type, allows setting a filter applied
  before aggregation, then the predicate to evaluate after aggregation
  - Example type: `String_array_aggregate_predicate_exp`
  - Usage:
    `{ filter_input: { where: { _gt: 1 } }, predicate: { _sum: { _gt: 2 } } }`
  - Configurable in OpenDD via
    `AggregateExpression.definition.graphql.aggregatePredicateInputTypeName`
    (scalar variant)

- `<object type>_aggregate_bool_exp` - Boolean expression over aggregations of
  properties of the object type
  - Example type: `InvoiceLine_aggregate_bool_exp`
  - Usage: `{ Quantity: { _sum: { _gt: 2 } } } }`
  - Configurable in OpenDD via
    `AggregateExpression.definition.graphql.aggregateBoolExpInputTypeName`
    (object variant)

- `<scalar type>_aggregate_bool_exp` - Application of aggregate functions and
  then applying a comparison to the result, plus boolean logic operators
  - Example type: `Int_aggregate_bool_exp`
  - Usage: `{ _sum: { _gt: 2 } } }`
  - Configurable in OpenDD via
    `AggregateExpression.definition.graphql.aggregateBoolExpInputTypeName`
    (scalar variant)

- `<scalar type>_<n-ary aggregate function>_aggregate_predicate_args` - Set the
  arguments to pass to the n-ary aggregate function and apply a comparison to
  the result
  - Example type: `String_concat_aggregate_predicate_args`
  - Usage: `{ args: { separator: ", " }, comparison: { _eq: "test" } }`
  - Configurable in OpenDD via
    `AggregationExpression.definition.operand.scalar.aggregationFunctions[].graphql.aggregatePredicateArgsInputTypeName`

```graphql
input Invoice_bool_exp {
  # Logic operators
  # WARN: Potential name conflicts
  _and: [Invoice_bool_exp!]
  _or: [Invoice_bool_exp!]
  _not: Invoice_bool_exp

  # Scalar fields
  InvoiceId: Int_bool_exp
  InvoiceDate: Date_bool_exp
  CustomerId: Int_bool_exp
  Total: Decimal_bool_exp

  # Nested objects/object relationships
  BillingAddress: Address_bool_exp
  Customer: Customer_bool_exp

  # Nested array of objects/array relationships
  InvoiceLines: InvoiceLine_bool_exp # Exists() array predicate
  # Configurable in OpenDD Relationship.definition.graphql.aggregateFieldName
  InvoiceLines_aggregate: InvoiceLine_aggregate_predicate_exp # WARN: Potential name conflict with another relationship/field
  Discounts: Discount_bool_exp
  # Configurable in OpenDD ObjectType.definition.fields[].graphql.aggregateFieldName
  Discounts_aggregate: Discount_aggregate_predicate_exp
}

input Int_bool_exp {
  # Logic operators
  _and: [Int_bool_exp!]
  _or: [Int_bool_exp!]
  _not: Int_bool_exp

  # Comparisons
  _eq: Int
  _gt: Int
  _gte: Int
  _in: [Int!]
  _is_null: Boolean
  _lt: Int
  _lte: Int
  _neq: Int
}

input Date_bool_exp {
  # Logic operators
  _and: [Date_bool_exp!]
  _or: [Date_bool_exp!]
  _not: Date_bool_exp

  # Comparisons
  _eq: Date
  _gt: Date
  _gte: Date
  _in: [Date!]
  _is_null: Boolean
  _lt: Date
  _lte: Date
  _neq: Date
}

input Decimal_bool_exp {
  # Logic operators
  _and: [Decimal_bool_exp!]
  _or: [Decimal_bool_exp!]
  _not: Decimal_bool_exp

  # Comparisons
  _eq: Decimal
  _gt: Decimal
  _gte: Decimal
  _in: [Decimal!]
  _is_null: Boolean
  _lt: Decimal
  _lte: Decimal
  _neq: Decimal
}

input Float_bool_exp {
  # Logic operators
  _and: [Float_bool_exp!]
  _or: [Float_bool_exp!]
  _not: Float_bool_exp

  # Comparisons
  _eq: Float
  _gt: Float
  _gte: Float
  _in: [Float!]
  _is_null: Boolean
  _lt: Float
  _lte: Float
  _neq: Float
}

input String_bool_exp {
  # Logic operators
  _and: [String_bool_exp!]
  _or: [String_bool_exp!]
  _not: String_bool_exp

  # Comparisons
  _eq: String
  _gt: String
  _gte: String
  _in: [String!]
  _is_null: Boolean
  _lt: String
  _lte: String
  _neq: String
}

input Address_bool_exp {
  # Logic operators
  # WARN: Potential name conflicts
  _and: [Address_bool_exp!]
  _or: [Address_bool_exp!]
  _not: Address_bool_exp

  # Scalar fields
  StreetAddress: String_bool_exp
  City: String_bool_exp
  State: String_bool_exp
  PostalCode: String_bool_exp
  Country: String_bool_exp
}

input Customer_bool_exp {
  # Logic operators
  # WARN: Potential name conflicts
  _and: [Customer_bool_exp!]
  _or: [Customer_bool_exp!]
  _not: Customer_bool_exp

  # Scalar fields
  CustomerId: Int_bool_exp
  FirstName: String_bool_exp
  LastName: String_bool_exp
  MobilePhone: String_bool_exp
  SupportRepId: Int_bool_exp

  # Nested objects/object relationships
  Address: Address_bool_exp

  # Nested array of objects/array relationships
  Invoices: Invoice_bool_exp # Exists() array predicate
  # Configurable in OpenDD Relationship.definition.graphql.aggregateFieldName
  Invoices_aggregate: Invoice_aggregate_predicate_exp # WARN: Potential name conflict with another relationship/field
  # Nested array of scalars
  Emails: String_bool_exp # Exists() array predicate
  # Configurable in OpenDD ObjectType.definition.fields[].graphql.aggregateFieldName
  Emails_aggregate: String_array_aggregate_predicate_exp
}

input Discount_bool_exp {
  # Logic operators
  # WARN: Potential name conflicts
  _and: [Discount_bool_exp!]
  _or: [Discount_bool_exp!]
  _not: Discount_bool_exp

  # Scalar fields
  Description: String_bool_exp
  Percentage: Decimal_bool_exp
}

input Invoice_aggregate_predicate_exp {
  filter_input: Invoice_filter_input # Filters the model before aggregating
  predicate: Invoice_aggregate_bool_exp!
}

input InvoiceLine_aggregate_predicate_exp {
  filter_input: InvoiceLine_filter_input # Filters the model before aggregating
  predicate: InvoiceLine_aggregate_bool_exp!
}

input Discount_aggregate_predicate_exp {
  filter_input: Discount_filter_input # Filters the model before aggregating
  predicate: Discount_aggregate_bool_exp!
}

input String_array_aggregate_predicate_exp {
  filter_input: String_filter_input
  predicate: String_aggregate_bool_exp
}

input InvoiceLine_bool_exp {
  # Logic operators
  # WARN: Potential name conflicts
  _and: [InvoiceLine_bool_exp!]
  _or: [InvoiceLine_bool_exp!]
  _not: InvoiceLine_bool_exp

  # Scalar fields
  InvoiceLineId: Int_bool_exp
  InvoiceId: Int_bool_exp
  TrackId: Int_bool_exp
  Quantity: Int_bool_exp

  # Nested objects/object relationships
  UnitPrice: Multicurrency_bool_exp
  Invoice: Invoice_bool_exp
}

input Multicurrency_bool_exp {
  # Logic operators
  # WARN: Potential name conflicts
  _and: [Multicurrency_bool_exp!]
  _or: [Multicurrency_bool_exp!]
  _not: Multicurrency_bool_exp

  # Scalar fields
  Currency: String_bool_exp
  Value: Decimal_bool_exp
}

input InvoiceLine_aggregate_bool_exp {
  # Logic operators
  # WARN: Potential name conflicts
  _and: [InvoiceLine_aggregate_bool_exp!]
  _or: [InvoiceLine_aggregate_bool_exp!]
  _not: InvoiceLine_aggregate_bool_exp

  # count all
  _count: Int_bool_exp

  # Scalar fields
  InvoiceLineId: Int_aggregate_bool_exp
  InvoiceId: Int_aggregate_bool_exp
  TrackId: Int_aggregate_bool_exp
  Quantity: Int_aggregate_bool_exp

  # Nested object fields & object relationships
  UnitPrice: Multicurrency_aggregate_bool_exp
  Invoice: Invoice_aggregate_bool_exp
}

input Int_aggregate_bool_exp {
  # Logic operators
  # WARN: Potential name conflicts
  _and: [Int_aggregate_bool_exp!]
  _or: [Int_aggregate_bool_exp!]
  _not: Int_aggregate_bool_exp

  # Aggregation functions
  _avg: Float_bool_exp
  _sum: Int_bool_exp
  _max: Int_bool_exp
  _min: Int_bool_exp
  _count: Int_bool_exp
  _count_distinct: Int_bool_exp
}

input Decimal_aggregate_bool_exp {
  # Logic operators
  # WARN: Potential name conflicts
  _and: [Decimal_aggregate_bool_exp!]
  _or: [Decimal_aggregate_bool_exp!]
  _not: Decimal_aggregate_bool_exp

  # Aggregation functions
  _avg: Decimal_bool_exp
  _sum: Decimal_bool_exp
  _max: Decimal_bool_exp
  _min: Decimal_bool_exp
  _count: Int_bool_exp
  _count_distinct: Int_bool_exp
}

input Date_aggregate_bool_exp {
  # Logic operators
  # WARN: Potential name conflicts
  _and: [Date_aggregate_bool_exp!]
  _or: [Date_aggregate_bool_exp!]
  _not: Date_aggregate_bool_exp

  # Aggregation functions
  _max: Date_bool_exp
  _min: Date_bool_exp
  _count: Int_bool_exp
  _count_distinct: Int_bool_exp
}

input String_aggregate_bool_exp {
  # Logic operators
  # WARN: Potential name conflicts
  _and: [String_aggregate_bool_exp!]
  _or: [String_aggregate_bool_exp!]
  _not: String_aggregate_bool_exp

  # Aggregation functions
  _max: String_bool_exp
  _min: String_bool_exp
  _count: Int_bool_exp
  _count_distinct: Int_bool_exp
  _concat: String_concat_aggregate_predicate_args
}

input String_concat_aggregate_predicate_args {
  args: String_concat_aggregate_args
  comparison: String_bool_exp
}

input Multicurrency_aggregate_bool_exp {
  # Logic operators
  # WARN: Potential name conflicts
  _and: [Multicurrency_aggregate_bool_exp!]
  _or: [Multicurrency_aggregate_bool_exp!]
  _not: Multicurrency_aggregate_bool_exp

  # Scalar fields
  Currency: String_aggregate_bool_exp
  Value: Decimal_aggregate_bool_exp
}

input Invoice_aggregate_bool_exp {
  # Logic operators
  # WARN: Potential name conflicts
  _and: [Invoice_aggregate_bool_exp!]
  _or: [Invoice_aggregate_bool_exp!]
  _not: Invoice_aggregate_bool_exp

  # count all
  _count: Int_bool_exp

  # Scalar fields
  InvoiceId: Int_aggregate_bool_exp
  InvoiceDate: Date_aggregate_bool_exp
  CustomerId: Int_aggregate_bool_exp
  Total: Decimal_aggregate_bool_exp

  # Nested object fields & object relationships
  BillingAddress: Address_aggregate_bool_exp
  Customer: Customer_aggregate_bool_exp
}

input Address_aggregate_bool_exp {
  # Logic operators
  # WARN: Potential name conflicts
  _and: [Address_aggregate_bool_exp!]
  _or: [Address_aggregate_bool_exp!]
  _not: Address_aggregate_bool_exp

  # count all
  _count: Int_bool_exp

  # Scalar fields
  StreetAddress: String_aggregate_bool_exp
  City: String_aggregate_bool_exp
  State: String_aggregate_bool_exp
  PostalCode: String_aggregate_bool_exp
  Country: String_aggregate_bool_exp
}

input Customer_aggregate_bool_exp {
  # Logic operators
  # WARN: Potential name conflicts
  _and: [Customer_aggregate_bool_exp!]
  _or: [Customer_aggregate_bool_exp!]
  _not: Customer_aggregate_bool_exp

  # count all
  _count: Int_bool_exp

  # Scalar fields
  CustomerId: Int_aggregate_bool_exp
  FirstName: String_aggregate_bool_exp
  LastName: String_aggregate_bool_exp
  MobilePhone: String_aggregate_bool_exp
  SupportRepId: Int_aggregate_bool_exp

  # Nested object fields
  Address: Address_aggregate_bool_exp
}

input Discount_aggregate_bool_exp {
  # Logic operators
  # WARN: Potential name conflicts
  _and: [Discount_aggregate_bool_exp!]
  _or: [Discount_aggregate_bool_exp!]
  _not: Discount_aggregate_bool_exp

  # count all
  _count: Int_bool_exp

  # Scalar fields
  Description: String_aggregate_bool_exp
  Percentage: Decimal_aggregate_bool_exp
}
```

### Aggregate Field Selector Types

Type categories:

- `<object type>_aggregate_fields` - Allows the selection of aggregations over
  fields on an object type
  - Example type: `Invoice_aggregate_fields`
  - Usage: `{ InvoiceId { _max } }`
  - Configurable in OpenDD:
    - Name: `AggregationExpression.definition.graphql.selectTypeName`
- `<scalar type>_aggregate_fields` - Allows the selection of aggregation
  functions to apply to a scalar-typed field
  - Example type: `Int_aggregate_fields`
  - Usage: `{ _sum }`
  - Configurable in OpenDD:
    - Name: `AggregationExpression.definition.graphql.selectTypeName`

```graphql
type Invoice_aggregate_fields {
  # Configurable in OpenDD in GraphqlConfig.query.aggregate.countFieldName
  _count: Int! # WARN: Potential name clash here
  # Scalar fields
  InvoiceId: Int_aggregate_fields!
  InvoiceDate: Date_aggregate_fields!
  CustomerId: Int_aggregate_fields!
  Total: Decimal_aggregate_fields!

  # Nested object fields
  BillingAddress: Address_aggregate_fields!
}

type Int_aggregate_fields {
  _avg: Float!
  _sum: Int!
  _max: Int!
  _min: Int!
  _count: Int! # Configurable in OpenDD in GraphqlConfig.query.aggregate.countFieldName
  _count_distinct: Int! # Configurable in OpenDD in GraphqlConfig.query.aggregate.countDistinctFieldName
}

type Date_aggregate_fields {
  _max: Date!
  _min: Date!
  _count: Int!
  _count_distinct: Int!
}

type Decimal_aggregate_fields {
  _avg: Decimal!
  _sum: Decimal!
  _max: Decimal!
  _min: Decimal!
  _count: Int!
  _count_distinct: Int!
}

type Address_aggregate_fields {
  _count: Int! # WARN: Potential name clash here
  # Scalar fields
  StreetAddress: String_aggregate_fields!
  City: String_aggregate_fields!
  State: String_aggregate_fields!
  PostalCode: String_aggregate_fields!
  Country: String_aggregate_fields!
}

type String_aggregate_fields {
  _max: String!
  _min: String!
  _count: Int!
  _count_distinct: Int!
  _concat(separator: String!): String!
}

type Customer_aggregate_fields {
  _count: Int! # WARN: Potential name clash here
  # Scalar fields
  CustomerId: Int_aggregate_fields!
  FirstName: String_aggregate_fields!
  LastName: String_aggregate_fields!
  Address: Address_aggregate_fields!
  MobilePhone: String_aggregate_fields!
  SupportRepId: Int_aggregate_fields!
}

type InvoiceLine_aggregate_fields {
  _count: Int! # WARN: Potential name clash here
  # Scalar fields
  InvoiceLineId: Int_aggregate_fields!
  InvoiceId: Int_aggregate_fields!
  TrackId: Int_aggregate_fields!
  Quantity: Int_aggregate_fields!

  # Nested object fields
  UnitPrice: Multicurrency_aggregate_fields!
}

type Multicurrency_aggregate_fields {
  _count: Int! # WARN: Potential name clash here
  # Scalar fields
  Currency: String_aggregate_fields!
  Value: Decimal_aggregate_fields!
}

type Discount_aggregate_fields {
  _count: Int! # WARN: Potential name clash here
  # Scalar fields
  Description: String_aggregate_fields!
  Percentage: Decimal_aggregate_fields!
}
```

### Group By Key Types

Type categories:

- `<object type>_grouping_key` - Allows the selection of either a scalar column
  or nesting into an object or array aggregation to use for a grouping key
  - Example type: `Invoice_grouping_key`
  - Usage: `{ _scalar_field: InvoiceDate }`
  - Configurable in OpenDD:
    - Name: `GroupsExpression.definition.graphql.groupKeyInputTypeName`
    - `_scalar_field` field name:
      `GraphqlConfig.definition.query.groups.scalarFieldFieldName`
    - Fields:
      `GroupsExpression.definition.groupableFields/groupableRelationships`
      (non-scalars)

- `<object type>_scalar_fields` - An enum of all scalar fields on the object
  type
  - Example type: `Invoice_scalar_fields`
  - Usage: `InvoiceDate`
  - Configurable in OpenDD:
    - Name: `GroupsExpression.definition.graphql.scalarFieldsEnumTypeName`
    - Enum members: `GroupsExpression.definition.groupableFields` (scalars)

- `<object type>_aggregate_select` - Allows the selection of an aggregate over a
  property on the object type
  - Example type: `InvoiceLine_aggregate_select`
  - Usage: `{ Quantity: { _unary_fn: _sum } }`
  - Configurable in OpenDD:
    - Name:
      `AggregateExpression.definition.graphql.aggregateSelectInputTypeName`
      (object variant)
    - Fields: `AggregateExpression.definition.operand.object.aggregatableFields`

- `<scalar type>_aggregate_select` - Allows the selection of an aggregation
  function for a scalar type
  - Example type: `String_aggregate_select`
  - Usage: `{ _unary_fn: _max }` or `{ _concat: { separator: ", " } }`
  - Configurable in OpenDD:
    - Name:
      `AggregateExpression.definition.graphql.aggregateSelectInputTypeName`
      (scalar variant)
    - Fields:
      `AggregateExpression.definition.operand.scalar.aggregationFunctions`
    - `_unary_fn`: GraphqlConfig.definition.query.aggregate.unaryFnFieldName

- `<scalar type>_aggregate_select_unary` - Enum of all unary aggregation
  functions for a scalar type
  - Example type: `String_aggregate_select_unary`
  - Usage: `_max`
  - Configurable in OpenDD:
    - Name:
      `AggregateExpression.definition.graphql.aggregateSelectUnaryFunctionEnumTypeName`
      (scalar variant)

```graphql
# For selecting what to group on for the Invoice model
input Invoice_grouping_key @oneOf {
  # WARN: Potential name clash here
  _scalar_field: Invoice_scalar_fields # Enum of all scalar fields - for grouping by scalar model fields
  # All object relationships/nested object fields on the Invoice model
  # For grouping by fields off of object-related model/nested objects
  BillingAddress: Address_grouping_key
  Customer: Customer_grouping_key

  # For grouping by aggregates of array-related model/nested arrays
  InvoiceLines_aggregate: InvoiceLine_aggregate_select
  Discounts_aggregate: Discount_aggregate_select
}

enum Invoice_scalar_fields {
  InvoiceId
  InvoiceDate
  CustomerId
  Total
}

input Address_grouping_key @oneOf {
  # WARN: Potential name clash here
  _scalar_field: Address_scalar_fields
}

enum Address_scalar_fields {
  StreetAddress
  City
  State
  PostalCode
  Country
}

input InvoiceLine_aggregate_select @oneOf {
  # Scalar fields
  InvoiceLineId: Int_aggregate_select
  InvoiceId: Int_aggregate_select
  TrackId: Int_aggregate_select
  Quantity: Int_aggregate_select

  # Nested object fields
  UnitPrice: Multicurrency_aggregate_select
}

input Int_aggregate_select @oneOf {
  _unary_fn: Int_aggregate_select_unary
}

enum Int_aggregate_select_unary {
  _count
  _count_distinct
  _sum
  _avg
  _max
  _min
}

input Multicurrency_aggregate_select @oneOf {
  # Scalar fields
  Currency: String_aggregate_select
  Value: Decimal_aggregate_select
}

input String_aggregate_select @oneOf {
  _unary_fn: String_aggregate_select_unary
  _concat: String_concat_aggregate_args
}

enum String_aggregate_select_unary {
  _count
  _count_distinct
  _max
  _min
}

input Decimal_aggregate_select @oneOf {
  _unary_fn: Decimal_aggregate_select_unary
}

enum Decimal_aggregate_select_unary {
  _count
  _count_distinct
  _sum
  _avg
  _max
  _min
}

input Invoice_aggregate_select @oneOf {
  # Scalar fields
  InvoiceId: Int_aggregate_select
  InvoiceDate: Date_aggregate_select
  CustomerId: Int_aggregate_select
  Total: Decimal_aggregate_select

  # Nested object fields/object relationships
  BillingAddress: Address_aggregate_select
}

input Date_aggregate_select @oneOf {
  _unary_fn: Date_aggregate_select_unary
}

enum Date_aggregate_select_unary {
  _count
  _count_distinct
  _max
  _min
}

input Address_aggregate_select @oneOf {
  # Scalar fields
  StreetAddress: String_aggregate_select
  City: String_aggregate_select
  State: String_aggregate_select
  PostalCode: String_aggregate_select
  Country: String_aggregate_select
}

input Discount_aggregate_select @oneOf {
  # Scalar fields
  Description: String_aggregate_select
  Percentage: Decimal_aggregate_select
}

input Customer_grouping_key @oneOf {
  # WARN: Potential name clash here
  _scalar_field: Customer_scalar_fields

  # Nested object
  Address: Address_grouping_key

  # Array relationships
  Invoices_aggregate: Invoice_aggregate_select

  # Array of scalars
  Emails_aggregate: String_aggregate_select
}

enum Customer_scalar_fields {
  CustomerId
  FirstName
  LastName
  MobilePhone
  SupportRepId
}

input InvoiceLine_grouping_key @oneOf {
  # WARN: Potential name clash here
  _scalar_field: InvoiceLine_scalar_fields # Enum of all scalar fields - for grouping by scalar model fields
  UnitPrice: Multicurrency_grouping_key
  Invoice: Invoice_grouping_key
}

enum InvoiceLine_scalar_fields {
  InvoiceLineId
  InvoiceId
  TrackId
  Quantity
}

input Multicurrency_grouping_key @oneOf {
  # WARN: Potential name clash here
  _scalar_field: Multicurrency_scalar_fields # Enum of all scalar fields - for grouping by scalar model fields
}

enum Multicurrency_scalar_fields {
  Currency
  Value
}
```

### Group By Order By Types

Type categories:

- `<object type>_grouping_order_by` - Allows the selection of ordering groups by
  either a part of the grouping key, or by an aggregation of the group
  - Example type: `Invoice_grouping_order_by`
  - Usage: `{ group_key: { InvoiceDate: Asc } }`
  - Configurable in OpenDD:
    - Name: `GroupsExpression.definition.graphql.groupsOrderByInputTypeName`
    - Field Names:
      `GraphqlConfig.definition.groups.groupKeyFieldName\groupAggregateFieldName`

```graphql
input Invoice_grouping_order_by {
  # Order by fields of the Invoice model, or into object/array relations
  # What's specified here must actually be part of the grouping key, or the query will fail
  # Name customizable in OpenDD: GraphqlConfig.definition.groups.groupKeyFieldName
  group_key: Invoice_order_by # Existing type
  # Order by an aggregation of the group
  # Name customizable in OpenDD: GraphqlConfig.definition.groups.groupAggregateFieldName
  group_aggregate: Invoice_aggregate_order_by # Existing type
}

input InvoiceLine_grouping_order_by {
  # Order by fields of the Invoice model, or into object/array relations
  # What's specified here must actually be part of the grouping key, or the query will fail
  # Name customizable in OpenDD: GraphqlConfig.definition.groups.groupKeyFieldName
  group_key: InvoiceLine_order_by # Existing type
  # Order by an aggregation of the group
  # Name customizable in OpenDD: GraphqlConfig.definition.groups.groupAggregateFieldName
  group_aggregate: InvoiceLine_aggregate_order_by # Existing type
}

input Customer_grouping_order_by {
  # Order by fields of the Customer model, or into object/array relations
  # What's specified here must actually be part of the grouping key, or the query will fail
  # Name customizable in OpenDD: GraphqlConfig.definition.groups.groupKeyFieldName
  group_key: Customer_order_by # Existing type
  # Order by an aggregation of the group
  # Name customizable in OpenDD: GraphqlConfig.definition.groups.groupAggregateFieldName
  group_aggregate: Customer_aggregate_order_by # Existing type
}
```

### Group By Field Selection

Type categories:

- `<object type>_groups` - Allows the selection of fields from the group key or
  objects from the group (or a further aggregation thereof)
  - Example type: `Invoice_groups`
  - Usage: `{ group_key { InvoiceDate } group_aggregate { _count }`
  - Configurable in OpenDD:
    - Name: `GroupsExpression.definition.graphql.groupsTypeName`
    - Field Names:
      `GraphqlConfig.definition.groups.groupKeyFieldName\groupAggregateFieldName`

- `<object type>_grouping_key_fields` - Allows the selection of fields that were
  used in the group key
  - Example type: `Invoice_grouping_key_fields`
  - Usage: `{ InvoiceDate }`
  - Configurable in OpenDD:
    - Name: `GroupsExpression.definition.graphql.groupKeyTypeName`
    - Fields:
      `GroupsExpression.definition.groupableFields/groupableRelationships`

```graphql
type Invoice_groups {
  # Name customizable in OpenDD: GraphqlConfig.definition.groups.groupKeyFieldName
  group_key: Invoice_grouping_key_fields!
  # Name customizable in OpenDD: GraphqlConfig.definition.groups.groupAggregateFieldName
  group_aggregate: Invoice_aggregate_fields!
}

# Very similar to the Invoice model type, except all fields are nullable
# since they can return null if the user selects a column that is not a
# part of the grouping key. Also, performing Rollup and Cube groupings
# will produce nulls for otherwise non-nullable fields. Object relationships
# use different types that follow the same pattern, too.
# Array relationships just use the _aggregate_fields types
type Invoice_grouping_key_fields {
  # Scalar fields, all nullable
  # Controlled in OpenDD via GroupsExpression.definition.groupableFields[]
  InvoiceId: Int
  InvoiceDate: Date
  CustomerId: Int
  Total: Decimal

  # Nested object fields/object relationships
  # Controlled in OpenDD via GroupsExpression.definition.groupableFields[].object
  BillingAddress: Address_grouping_key_fields!
  # Controlled in OpenDD via GroupsExpression.definition.groupableRelationships[].object
  Customer: Customer_grouping_key_fields!

  # Array relationship/nested array of objects
  # Controlled in OpenDD via GroupsExpression.definition.groupableRelationships[].aggregate
  InvoiceLines_aggregate: InvoiceLine_aggregate_fields!
  # Controlled in OpenDD via GroupsExpression.definition.groupableFields[].aggregate
  Discounts_aggregate: Discount_aggregate_fields!
}

type Address_grouping_key_fields {
  # Scalar fields, all nullable
  # Controlled in OpenDD via GroupsExpression.definition.groupableFields[]
  StreetAddress: String
  City: String
  State: String
  PostalCode: String
  Country: String
}

type Customer_grouping_key_fields {
  # Scalar fields, all nullable
  # Controlled in OpenDD via GroupsExpression.definition.groupableFields[]
  CustomerId: Int
  FirstName: String
  LastName: String
  MobilePhone: String
  SupportRepId: Int

  # Nested object fields/object relationships
  # Controlled in OpenDD via GroupsExpression.definition.groupableFields[].object
  Address: Address_grouping_key_fields!

  # Nested array of objects/array relationships
  # Controlled in OpenDD via GroupsExpression.definition.groupableRelationships[].aggregate
  Invoices_aggregate: Invoice_grouping_key_fields!

  # Array of scalars
  # Controlled in OpenDD via GroupsExpression.definition.groupableFields[].aggregate
  Emails_aggregate: String_aggregate_fields!
}

type InvoiceLine_groups {
  # Name customizable in OpenDD: GraphqlConfig.definition.groups.groupKeyFieldName
  group_key: InvoiceLine_grouping_key_fields!
  # Name customizable in OpenDD: GraphqlConfig.definition.groups.groupAggregateFieldName
  group_aggregate: InvoiceLine_aggregate_fields!
}

type InvoiceLine_grouping_key_fields {
  # Scalar fields, all nullable
  # Controlled in OpenDD via GroupsExpression.definition.groupableFields[]
  InvoiceLineId: Int
  InvoiceId: Int
  TrackId: Int
  Quantity: Int

  # Nested object fields/object relationships
  # Controlled in OpenDD via GroupsExpression.definition.groupableFields[].object
  UnitPrice: Multicurrency_grouping_key_fields!
  # Controlled in OpenDD via GroupsExpression.definition.groupableRelationships[].object
  Invoice: Invoice_grouping_key_fields!
}

type Multicurrency_grouping_key_fields {
  # Scalar fields, all nullable
  # Controlled in OpenDD via GroupsExpression.definition.groupableFields[]
  Currency: String
  Value: Decimal
}

type Customer_groups {
  # Name customizable in OpenDD: GraphqlConfig.definition.groups.groupKeyFieldName
  group_key: Customer_grouping_key_fields!
  # Name customizable in OpenDD: GraphqlConfig.definition.groups.groupAggregateFieldName
  group_aggregate: Customer_aggregate_fields!
}
```

## Open DD Changes

### Graphql Config

The GraphqlConfig object can be used to customize global naming of `_aggregate`
and `_groups` field argument names. It can also be used to customize the
different grouping type enums (ie. Standard, Rollup, Cube, etc).

```yaml
kind: GraphqlConfig
version: v1
definition:
  query:
    rootOperationTypeName: Query
    argumentsInput:
      fieldName: args
    limitInput:
      fieldName: limit
    offsetInput:
      fieldName: offset
    filterInput:
      fieldName: where
      operatorNames:
        and: _and
        or: _or
        not: _not
        isNull: _is_null
    orderByInput:
      fieldName: order_by
      enumDirectionValues:
        asc: Asc
        desc: Desc
      enumTypeNames:
        - directions:
            - Asc
            - Desc
          typeName: OrderBy
    # New!
    aggregate:
      filterInputFieldName: filter_input
      countFieldName: _count
      countDistinctFieldName: _count_distinct
      unaryFnFieldName: _unary_fn
    # New!
    groups:
      filterInputFieldName: filter_input
      groupingKeysFieldName: grouping_keys
      groupingTypeFieldName: grouping_type
      havingFieldName: having
      groupKeyFieldName: group_key
      groupAggregateFieldName: group_aggregate
      scalarFieldFieldName: _scalar_field
      groupingType:
        enumValues:
          standard: Standard
          rollup: Rollup
          cube: Cube
        groupingTypeEnumTypeNames:
          - groupingTypes: [Standard, Rollup, Cube]
            typeName: GroupingType

  mutation:
    rootOperationTypeName: Mutation
```

### Model & Object Type

The `Model` is where the `filter_input` type name can be configured for that
model, and also where the `AggregateExpression` is specified that defines how to
aggregate the model via its root field. The root field name is also specified
here.

On the `ObjectType` associated with the model, one can specify how to aggregate
any nested array field, again using an `AggregationExpression` and the naming of
the "computed" field that is added that represents the aggregation of the nested
array field.

```yaml
kind: Model
version: v1
definition:
  name: Invoice
  objectType: Invoice
  source:
    dataConnectorName: app_connector
    collection: Invoice
  filterExpressionType: InvoiceBoolExp
  orderByExpression: InvoiceOrderByExp
  # New! Specify the aggregation expression used to configure the shape of aggregation for the root field
  aggregateExpression: Invoice_aggregate_exp
  # New! Specify the groups expression used to configure the shape of grouping for the root field
  groupsExpression: Invoice_groups_exp
  graphql:
    # New! The type name to use for the `filter_input` type
    filterInputTypeName: Invoice_filter_input
    # New! The root field name to use for the aggregate root field
    aggregate:
      queryRootField: Invoice_aggregate
    # New! The root field name to use for the groups root field
    groups:
      queryRootField: Invoice_groups
    selectMany:
      queryRootField: Invoice
    selectUniques:
      - queryRootField: InvoiceByInvoiceId
        uniqueIdentifier:
          - InvoiceId

kind: ObjectType
version: v1
definition:
  name: Invoice
  fields:
    - name: InvoiceId
      type: Int!
    - name: InvoiceDate
      type: Date!
    - name: CustomerId
      type: Int!
    - name: Discounts
      type: [Discount!]!
      # New: Aggregations over nested arrays
      aggregate:
        aggregateExpression: Discount_aggregate_exp
        description: Aggregate over this invoice's discounts
      graphql:
        aggregateFieldName: Discounts_aggregate # This is effectively computed field added to the object type
    - name: BillingAddress
      type: Address
    - name: Total
      type: Decimal!
  graphql:
    typeName: App_Invoice
    inputTypeName: App_InvoiceInput
  dataConnectorTypeMapping:
    - dataConnectorName: app_connector
      dataConnectorObjectType: Invoice
      fieldMapping:
        InvoiceId:
          column:
            name: InvoiceId
        InvoiceDate:
          column:
            name: InvoiceDate
        CustomerId:
          column:
            name: CustomerId
        Discounts:
          column:
            name: Discounts
        BillingAddress:
          column:
            name: BillingAddress
        Total:
          column:
            name: Total
```

### Relationship

For array relationships, we can now add an additional aggregate field that is
linked to a target `AggregateExpression`.

```yaml
kind: Relationship
version: v1
definition:
  name: Invoices
  source: Customer
  target:
    model:
      name: Invoice
      relationshipType: Array
      # New - only applies to `relationshipType: Array`
      # TODO: This sucks living in here, but relationshipType is not a top level discriminator
      # and these fields only apply to models and that's where the type variant is applied
      aggregate:
        aggregateExpression: Invoice_aggregate_exp
        description: Aggregate over the customer's invoices
      groups:
        groupsExpression: Invoice_groups_exp
        description: Group over the customer's invoices

  mapping:
    - source:
        fieldPath:
          - fieldName: CustomerId
      target:
        modelField:
          - fieldName: CustomerId
  # New!
  graphql:
    aggregateFieldName: Invoices_aggregate
    groupsFieldName: Invoices_groups
```

### AggregateExpression (NEW!)

This is a new OpenDD kind that represents how to aggregate either an object type
or a scalar type. Because aggregation cross-cuts ordering and filtering, the
GraphQL type names for aggregation-specific ordering and predicate types is
captured here.

#### Object types

For object types, one can specify the `aggregatableFields`, and for each field
the `AggregationExpression` that can be used to aggregate it, depending on the
field's type (ie. an object `AggregationExpression` for object-typed fields, and
a scalar `AggregationExpression` for scalar-typed fields).

One can also enable or disable the `count`/`countDistinct` special-cased
aggregations. However, the `countDistinct` function can only be used if the the
`AggregateExpression` is not used on a Model, because you can't "distinctly"
count rows in a collection (eg. `COUNT(*)` and `COUNT(DISTINCT *)` is the same).
Whether or not distinct counts are possible will be need to be exposed in
connector capabilities.

```yaml
kind: AggregateExpression
version: v1
definition:
  name: Invoice_aggregate_exp # Unique only to AggregateExpressions
  operand:
    object:
      aggregatedType: Invoice
      aggregatableFields:
        - fieldName: InvoiceId
          description: Aggregates over the InvoiceId field
          aggregateExpression: Int_aggregate_exp
        # Only nested objects are supported for now. Object relationships are not.
        - fieldName: BillingAddress
          description: Aggregates over the BillingAddress field
          aggregateExpression: Address_aggregate_exp
  # count and countDistinct are special cased because "count" doesn't evaluate to a scalar/object type
  # but rather the "nullability" of a type
  count:
    enable: true
    description: Count of all invoices
    booleanExpression: Int_comparison_exp
  countDistinct: # Only enable-able if the AggregateExpression is not used on a Model
    enable: true
    description: Distinct count of all invoices
    booleanExpression: Int_comparison_exp
  graphql:
    selectTypeName: Invoice_aggregate_fields
    orderByInputTypeName: Invoice_aggregate_order_by
    aggregatePredicateInputTypeName: Invoice_aggregate_predicate_exp
    aggregateBoolExpInputTypeName: Invoice_aggregate_bool_exp
    aggregateSelectInputTypeName: Invoice_aggregate_select
    aggregateSelectUnaryFunctionEnumTypeName: Invoice_aggregate_select_unary # Unused on object variants for now
```

#### Scalar types

For scalar types, one specifies the aggregation functions that can be used to
aggregate the scalar type (`aggregationFunctions`). For each function, you must
specify the function return type (as this may not match input scalar type) and a
`BooleanExpression` to use when generating aggregation predicates against the
result of that aggregation function.

If the aggregation function takes additional arguments, one specifies the
arguments to the aggregate function in the OpenDD representation.

The aggregation functions are mapped to data connectors via
`dataConnectorAggregationFunctionMapping`. If there are arguments, these
arguments can be hardcoded to preset values here, which can obviate the need for
arguments to be defined in the OpenDD representation if all the arguments are
hardcoded.

```yaml
kind: AggregateExpression
version: v1
definition:
  name: String_aggregate_exp # Unique only to AggregateExpressions
  operand:
    scalar:
      aggregatedType: String
      aggregationFunctions:
        - name: _min # Name you want to give the function in OpenDD and GraphQL
          returnType: String! # This is an OpenDD type
          description: Returns the lexicographically least string
          # The boolean expression to use to compare against the aggregated return value
          # Omit to remove from aggregation predicates
          booleanExpression: String_comparison_exp
        - name: _max
          returnType: String!
          description: Returns the lexicographically most string
          booleanExpression: String_comparison_exp
        - name: _concat
          arguments:
            - name: separator
              type: String!
          graphql:
            argsInputTypeName: String_concat_aggregate_args # `{ separator: ", " }`
            orderByArgsInputTypeName: String_concat_aggregate_order_by # `{ args: { separator: ", " }, ordering: Asc }`
            aggregatePredicateArgsInputTypeName: String_concat_aggregate_predicate_args # `{ args: { separator: ", " }, comparison: { _eq: "test" } }`
          returnType: String!
          description: Concatenates all string together with a separator
          booleanExpression: String_comparison_exp
        - name: _concat_comma
          # This doesn't need arguments, since all the arguments are preset in the
          returnType: String!
          description: Concatenates all strings together with a comma
          booleanExpression: String_comparison_exp
      dataConnectorAggregationFunctionMapping:
        - dataConnectorName: pg_1
          dataConnectorScalarType: text
          functionMapping:
            _min: # OpenDD function name
              name: min # Data connector aggregation function name
            _max:
              name: max
            _concat:
              name: concat
              argumentMapping:
                # OpenDD -> NDC
                separator: separator
            _concat_comma:
              name: count
              argumentPresets:
                - argument: separator
                  value:
                    literal: ","
  # count and countDistinct are special cased because "count" doesn't evaluate to a scalar/object type
  # but rather the "nullability" of a type
  count:
    enable: true
    description: Counts all non-null Ints
    booleanExpression: Int_comparison_exp
  countDistinct:
    enable: true
    description: Counts all distinct non-null Ints
    booleanExpression: Int_comparison_exp
  graphql:
    selectTypeName: String_aggregate_fields
    orderByInputTypeName: String_aggregate_order_by
    aggregatePredicateInputTypeName: String_array_aggregate_predicate_exp
    aggregateBoolExpInputTypeName: String_aggregate_bool_exp
    aggregateSelectInputTypeName: String_aggregate_select
    aggregateSelectUnaryFunctionEnumTypeName: String_aggregate_select_unary
```

### TypePermissions

There are no changes to `TypePermissions` but it is worth noting that any field
that is not allowed in a Type's permissions, should also not be allowed to be
used in grouping or aggregations.

```yaml
kind: TypePermissions
version: v1
definition:
  typeName: Invoice
  permissions:
    - role: admin
      output:
        allowedFields:
          - InvoiceId
          - CustomerId
          - InvoiceDate
          - BillingAddress
          - Total
```

### OrderByExpression

In order to enable ordering by aggregates, the `OrderByExpression` has been
modified to expose configuration of aggregates over nested array fields (under
`orderableFields`) and over relationships (under `orderableRelationships`).

```yaml
kind: OrderByExpression
version: v1
definition:
  name: Invoice_order_by_exp
  orderedType: Invoice
  orderableFields:
    - fieldName: InvoiceId
      enableOrderByDirections: [Asc, Desc]
    - fieldName: CustomerId
      enableOrderByDirections: [Asc]
    - fieldName: BillingAddress
      orderByExpression: Address_order_by_default_exp
    # Nested array field
    - fieldName: Discounts
      aggregateExpression: Discount_aggregate_exp
  orderableRelationships:
    - relationshipName: Customer
      orderByExpression: Customer_order_by_exp
    # New for array relationships! Enables ordering by aggregations across this relationship
    - relationshipName: InvoiceLines
      aggregateExpression: InvoiceLine_aggregate_exp

  graphql:
    expressionTypeName: Customer_order_by
```

### BooleanExpressionType (object variant)

In order to enable filtering by the results of applying an aggregate, the
`BooleanExpressionType`s for object types has been modified to expose aggregate
configuration. You can configure aggregation of nested array fields in
`comparableFields` and aggregation of array relationships in
`comparableRelationships`.

```yaml
kind: BooleanExpressionType
version: v2
definition:
  name: Album_bool_exp
  operand:
    object:
      type: Invoice
      comparableFields:
        - fieldName: InvoiceId
          booleanExpressionType: pg_Int_Comparison_exp
        - fieldName: CustomerId
          booleanExpressionType: pg_Int_Comparison_exp_with_is_null
        - fieldName: BillingAddress
          booleanExpressionType: Address_bool_exp
        # Nested array field
        - fieldName: Discount
          booleanExpressionType: Discount_bool_exp # Exists() bool exp
          # New! Only for nested array fields. Enables aggregation predicates
          aggregateExpression: Discount_aggregate_exp
      comparableRelationships:
        - relationshipName: Customer
          booleanExpressionType: Customer_bool_exp
        # Array relationship
        - relationshipName: InvoiceLines
          booleanExpressionType: InvoiceLine_bool_exp # Exists() bool exp
          # New! Only for array relationships. Enables aggregation predicates
          aggregateExpression: InvoiceLine_aggregate_exp
  logicalOperators:
    enable: true
  isNull:
    enable: true
  graphql:
    typeName: App_Album_bool_exp
```

### GroupsExpression

`GroupsExpressions` are definable for `ObjectType`s and are associated with from
a `Model` and a `Relationship`. They define how a collection of an object type
can be grouped (ie. which fields/relationships)

```yaml
kind: GroupsExpression
version: v1
definition:
  name: Invoice_groups_exp # Unique only to GroupsExpressions
  objectType: Invoice
  groupableFields:
    - fieldName: InvoiceId
    - fieldName: InvoiceDate
    - fieldName: CustomerId
    - fieldName: Total
    # Nested object field
    - fieldName: BillingAddress
      groupsExpression: Address_groups_exp
    # Nested array field
    - fieldName: Discounts
      aggregateExpression: Discount_aggregate_exp # Required to know how to aggregate this type
  groupableRelationships:
    # Object relationship
    - relationshipName: Customer
      groupsExpression: Customer_groups_exp
    # Array relationship
    - relationshipName: InvoiceLines
      aggregateExpression: InvoiceLine_aggregate_exp # Required to know how to aggregate this type
  graphql:
    groupsTypeName: Invoice_groups
    groupKeyTypeName: Invoice_grouping_key_fields
    groupKeyInputTypeName: Invoice_grouping_key
    scalarFieldsEnumTypeName: Invoice_scalar_fields
    groupsOrderByInputTypeName: Invoice_grouping_order_by
```

## Discarded Considerations

### distinct_on

`distinct_on` allows you to group by a set of fields and for each row group,
take the first row. We decided to cut `distinct_on` from scope, as it is a very
Postgres function. We might be able to offer a workaround by using group by with
a "first" windowing function instead.

This was the proposed GraphQL schema for `distinct_on`:

```graphql
# Existing query root type
type query_root {
  Invoice_aggregate(
    distinct_on: [Invoice_select_column!]
    limit: Int
    offset: Int
    order_by: [Invoice_order_by!]
    where: Invoice_bool_exp
  ): Invoice_aggregate!
}

# Allows the selection of a column. Does not allow the selection of an aggregation of an
# array relationship/nested array or navigation into object relationships
input Invoice_select_column @oneOf {
  scalar: Invoice_scalar_fields
  object: Invoice_object_fields
}

# All scalar fields in the model
enum Invoice_scalar_fields {
  InvoiceId
  InvoiceDate
  CustomerId
  Total
}

input Invoice_object_fields @oneOf {
  BillingAddress: Address_select_column
}

input Address_select_column @oneOf {
  scalar: Address_scalar_fields
  # No nested object fields, object property omitted
}

enum Address_scalar_fields {
  StreetAddress
  City
  State
  PostalCode
  Country
}
```

### n-ary functions that take columns as additional arguments

n-ary functions are aggregate functions that operate over a single column, but
also take additional arguments (ie. they take two or more arguments, with one
being the main column they operate over).

- `WEIGHTED_AVERAGE(column, weight)` - The weighted average function has been
  chosen as a simple example of a function that takes two column arguments, one
  is the column to average, the other is a weighting factor applied to each
  value to be averaged. The weighting factor would be drawn from another column.

n-ary aggregate functions that can take a column in the arguments make a huge
mess of the types, as they require embedding a column selector type that is
specific to the aggregate root table. This results in an explosion of types,
which is especially hard to handle in v3 because of needing to explicitly name
all types in the metadata. Example:

```graphql
input InvoiceLine_aggregate_order_by @oneOf {
  # Scalar fields
  InvoiceLineId: InvoiceLine_Int_aggregate_order_by # Per model, per scalar type because of n-ary aggregate args that can take model columns
}

input InvoiceLine_Int_aggregate_order_by @oneOf {
  # n-ary aggregate functions
  weighted_average: InvoiceLine_Int_aggregate_weighted_average_order_by # Per model, per scalar type because of n-ary aggregate args that can take model columns
}

input InvoiceLine_Int_aggregate_weighted_average_order_by {
  order: order_by!
  args: InvoiceLine_Int_aggregate_weighted_average_args!
}

input InvoiceLine_Int_aggregate_weighted_average_args {
  weight: InvoiceLine_select_column! # TODO: Dangerous, allows selection of columns of incorrect scalar type, may need a filtered variant of _select_column types 🤮
}
```

We decided to cut these from scope and direct users to use native queries if
they need to use advanced aggregate functions like this. Computed fields may
also be another workaround for this use case.

### Forall predicates for filtering by array related model

Currently we support filtering by an array related model using "exists"
semantics (ie. filter Invoices where there exists an invoice line with a
quantity greater than 1). We could add forall semantics as another feature (ie.
filter Invoices where all invoice lines have a quantity greater than 1).

However, we decided we have a good enough workaround to avoid adding this
feature: use a negated exists query instead (ie. filter Invoices where there
does not exist a invoice line with a quantity less than or equal to 1).

### Aggregates of aggregates

We could potentially support the ability to aggregate over aggregates. However,
we decided to cut this from scope, as it was not supported in v2. Users can use
native queries to perform complex aggregations that are not supported for now.
