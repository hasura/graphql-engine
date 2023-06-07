export const chinook1 = `
schema {
  query: query_root
  mutation: mutation_root
  subscription: subscription_root
}

"""whether this query should be cached (Hasura Cloud only)"""
directive @cached(
  """measured in seconds"""
  ttl: Int! = 60

  """refresh the cache entry"""
  refresh: Boolean! = false
) on QUERY

"""
columns and relationships of "Album"
"""
type Album {
  AlbumId: Int!

  """An object relationship"""
  Artist: Artist!
  ArtistId: Int!
  Title: String!

  """An array relationship"""
  Tracks(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): [Track!]!

  """An aggregate relationship"""
  Tracks_aggregate(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): Track_aggregate!
}

"""
aggregated selection of "Album"
"""
type Album_aggregate {
  aggregate: Album_aggregate_fields
  nodes: [Album!]!
}

input Album_aggregate_bool_exp {
  count: Album_aggregate_bool_exp_count
}

input Album_aggregate_bool_exp_count {
  arguments: [Album_select_column!]
  distinct: Boolean
  filter: Album_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "Album"
"""
type Album_aggregate_fields {
  avg: Album_avg_fields
  count(columns: [Album_select_column!], distinct: Boolean): Int!
  max: Album_max_fields
  min: Album_min_fields
  stddev: Album_stddev_fields
  stddev_pop: Album_stddev_pop_fields
  stddev_samp: Album_stddev_samp_fields
  sum: Album_sum_fields
  var_pop: Album_var_pop_fields
  var_samp: Album_var_samp_fields
  variance: Album_variance_fields
}

"""
order by aggregate values of table "Album"
"""
input Album_aggregate_order_by {
  avg: Album_avg_order_by
  count: order_by
  max: Album_max_order_by
  min: Album_min_order_by
  stddev: Album_stddev_order_by
  stddev_pop: Album_stddev_pop_order_by
  stddev_samp: Album_stddev_samp_order_by
  sum: Album_sum_order_by
  var_pop: Album_var_pop_order_by
  var_samp: Album_var_samp_order_by
  variance: Album_variance_order_by
}

"""
input type for inserting array relation for remote table "Album"
"""
input Album_arr_rel_insert_input {
  data: [Album_insert_input!]!

  """upsert condition"""
  on_conflict: Album_on_conflict
}

"""aggregate avg on columns"""
type Album_avg_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by avg() on columns of table "Album"
"""
input Album_avg_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""
Boolean expression to filter rows from the table "Album". All fields are combined with a logical 'AND'.
"""
input Album_bool_exp {
  AlbumId: Int_comparison_exp
  Artist: Artist_bool_exp
  ArtistId: Int_comparison_exp
  Title: String_comparison_exp
  Tracks: Track_bool_exp
  Tracks_aggregate: Track_aggregate_bool_exp
  _and: [Album_bool_exp!]
  _not: Album_bool_exp
  _or: [Album_bool_exp!]
}

"""
unique or primary key constraints on table "Album"
"""
enum Album_constraint {
  """
  unique or primary key constraint on columns "AlbumId"
  """
  PK_Album
}

"""
input type for incrementing numeric columns in table "Album"
"""
input Album_inc_input {
  AlbumId: Int
  ArtistId: Int
}

"""
input type for inserting data into table "Album"
"""
input Album_insert_input {
  AlbumId: Int
  Artist: Artist_obj_rel_insert_input
  ArtistId: Int
  Title: String
  Tracks: Track_arr_rel_insert_input
}

"""aggregate max on columns"""
type Album_max_fields {
  AlbumId: Int
  ArtistId: Int
  Title: String
}

"""
order by max() on columns of table "Album"
"""
input Album_max_order_by {
  AlbumId: order_by
  ArtistId: order_by
  Title: order_by
}

"""aggregate min on columns"""
type Album_min_fields {
  AlbumId: Int
  ArtistId: Int
  Title: String
}

"""
order by min() on columns of table "Album"
"""
input Album_min_order_by {
  AlbumId: order_by
  ArtistId: order_by
  Title: order_by
}

"""
response of any mutation on the table "Album"
"""
type Album_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Album!]!
}

"""
input type for inserting object relation for remote table "Album"
"""
input Album_obj_rel_insert_input {
  data: Album_insert_input!

  """upsert condition"""
  on_conflict: Album_on_conflict
}

"""
on_conflict condition type for table "Album"
"""
input Album_on_conflict {
  constraint: Album_constraint!
  update_columns: [Album_update_column!]! = []
  where: Album_bool_exp
}

"""Ordering options when selecting data from "Album"."""
input Album_order_by {
  AlbumId: order_by
  Artist: Artist_order_by
  ArtistId: order_by
  Title: order_by
  Tracks_aggregate: Track_aggregate_order_by
}

"""primary key columns input for table: Album"""
input Album_pk_columns_input {
  AlbumId: Int!
}

"""
select columns of table "Album"
"""
enum Album_select_column {
  """column name"""
  AlbumId

  """column name"""
  ArtistId

  """column name"""
  Title
}

"""
input type for updating data in table "Album"
"""
input Album_set_input {
  AlbumId: Int
  ArtistId: Int
  Title: String
}

"""aggregate stddev on columns"""
type Album_stddev_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by stddev() on columns of table "Album"
"""
input Album_stddev_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""aggregate stddev_pop on columns"""
type Album_stddev_pop_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by stddev_pop() on columns of table "Album"
"""
input Album_stddev_pop_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""aggregate stddev_samp on columns"""
type Album_stddev_samp_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by stddev_samp() on columns of table "Album"
"""
input Album_stddev_samp_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""
Streaming cursor of the table "Album"
"""
input Album_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Album_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Album_stream_cursor_value_input {
  AlbumId: Int
  ArtistId: Int
  Title: String
}

"""aggregate sum on columns"""
type Album_sum_fields {
  AlbumId: Int
  ArtistId: Int
}

"""
order by sum() on columns of table "Album"
"""
input Album_sum_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""
update columns of table "Album"
"""
enum Album_update_column {
  """column name"""
  AlbumId

  """column name"""
  ArtistId

  """column name"""
  Title
}

input Album_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Album_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Album_set_input

  """filter the rows which have to be updated"""
  where: Album_bool_exp!
}

"""aggregate var_pop on columns"""
type Album_var_pop_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by var_pop() on columns of table "Album"
"""
input Album_var_pop_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""aggregate var_samp on columns"""
type Album_var_samp_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by var_samp() on columns of table "Album"
"""
input Album_var_samp_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""aggregate variance on columns"""
type Album_variance_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by variance() on columns of table "Album"
"""
input Album_variance_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""
columns and relationships of "Artist"
"""
type Artist {
  """An array relationship"""
  Albums(
    """distinct select on columns"""
    distinct_on: [Album_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Album_order_by!]

    """filter the rows returned"""
    where: Album_bool_exp
  ): [Album!]!

  """An aggregate relationship"""
  Albums_aggregate(
    """distinct select on columns"""
    distinct_on: [Album_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Album_order_by!]

    """filter the rows returned"""
    where: Album_bool_exp
  ): Album_aggregate!
  ArtistId: Int!
  Name: String
}

"""
aggregated selection of "Artist"
"""
type Artist_aggregate {
  aggregate: Artist_aggregate_fields
  nodes: [Artist!]!
}

"""
aggregate fields of "Artist"
"""
type Artist_aggregate_fields {
  avg: Artist_avg_fields
  count(columns: [Artist_select_column!], distinct: Boolean): Int!
  max: Artist_max_fields
  min: Artist_min_fields
  stddev: Artist_stddev_fields
  stddev_pop: Artist_stddev_pop_fields
  stddev_samp: Artist_stddev_samp_fields
  sum: Artist_sum_fields
  var_pop: Artist_var_pop_fields
  var_samp: Artist_var_samp_fields
  variance: Artist_variance_fields
}

"""aggregate avg on columns"""
type Artist_avg_fields {
  ArtistId: Float
}

"""
Boolean expression to filter rows from the table "Artist". All fields are combined with a logical 'AND'.
"""
input Artist_bool_exp {
  Albums: Album_bool_exp
  Albums_aggregate: Album_aggregate_bool_exp
  ArtistId: Int_comparison_exp
  Name: String_comparison_exp
  _and: [Artist_bool_exp!]
  _not: Artist_bool_exp
  _or: [Artist_bool_exp!]
}

"""
unique or primary key constraints on table "Artist"
"""
enum Artist_constraint {
  """
  unique or primary key constraint on columns "ArtistId"
  """
  PK_Artist
}

"""
input type for incrementing numeric columns in table "Artist"
"""
input Artist_inc_input {
  ArtistId: Int
}

"""
input type for inserting data into table "Artist"
"""
input Artist_insert_input {
  Albums: Album_arr_rel_insert_input
  ArtistId: Int
  Name: String
}

"""aggregate max on columns"""
type Artist_max_fields {
  ArtistId: Int
  Name: String
}

"""aggregate min on columns"""
type Artist_min_fields {
  ArtistId: Int
  Name: String
}

"""
response of any mutation on the table "Artist"
"""
type Artist_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Artist!]!
}

"""
input type for inserting object relation for remote table "Artist"
"""
input Artist_obj_rel_insert_input {
  data: Artist_insert_input!

  """upsert condition"""
  on_conflict: Artist_on_conflict
}

"""
on_conflict condition type for table "Artist"
"""
input Artist_on_conflict {
  constraint: Artist_constraint!
  update_columns: [Artist_update_column!]! = []
  where: Artist_bool_exp
}

"""Ordering options when selecting data from "Artist"."""
input Artist_order_by {
  Albums_aggregate: Album_aggregate_order_by
  ArtistId: order_by
  Name: order_by
}

"""primary key columns input for table: Artist"""
input Artist_pk_columns_input {
  ArtistId: Int!
}

"""
select columns of table "Artist"
"""
enum Artist_select_column {
  """column name"""
  ArtistId

  """column name"""
  Name
}

"""
input type for updating data in table "Artist"
"""
input Artist_set_input {
  ArtistId: Int
  Name: String
}

"""aggregate stddev on columns"""
type Artist_stddev_fields {
  ArtistId: Float
}

"""aggregate stddev_pop on columns"""
type Artist_stddev_pop_fields {
  ArtistId: Float
}

"""aggregate stddev_samp on columns"""
type Artist_stddev_samp_fields {
  ArtistId: Float
}

"""
Streaming cursor of the table "Artist"
"""
input Artist_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Artist_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Artist_stream_cursor_value_input {
  ArtistId: Int
  Name: String
}

"""aggregate sum on columns"""
type Artist_sum_fields {
  ArtistId: Int
}

"""
update columns of table "Artist"
"""
enum Artist_update_column {
  """column name"""
  ArtistId

  """column name"""
  Name
}

input Artist_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Artist_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Artist_set_input

  """filter the rows which have to be updated"""
  where: Artist_bool_exp!
}

"""aggregate var_pop on columns"""
type Artist_var_pop_fields {
  ArtistId: Float
}

"""aggregate var_samp on columns"""
type Artist_var_samp_fields {
  ArtistId: Float
}

"""aggregate variance on columns"""
type Artist_variance_fields {
  ArtistId: Float
}

"""
columns and relationships of "Customer"
"""
type Customer {
  Address: String
  City: String
  Company: String
  Country: String
  CustomerId: Int!
  Email: String!

  """An object relationship"""
  Employee: Employee
  Fax: String
  FirstName: String!

  """An array relationship"""
  Invoices(
    """distinct select on columns"""
    distinct_on: [Invoice_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Invoice_order_by!]

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): [Invoice!]!

  """An aggregate relationship"""
  Invoices_aggregate(
    """distinct select on columns"""
    distinct_on: [Invoice_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Invoice_order_by!]

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): Invoice_aggregate!
  LastName: String!
  Phone: String
  PostalCode: String
  State: String
  SupportRepId: Int
}

"""
aggregated selection of "Customer"
"""
type Customer_aggregate {
  aggregate: Customer_aggregate_fields
  nodes: [Customer!]!
}

input Customer_aggregate_bool_exp {
  count: Customer_aggregate_bool_exp_count
}

input Customer_aggregate_bool_exp_count {
  arguments: [Customer_select_column!]
  distinct: Boolean
  filter: Customer_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "Customer"
"""
type Customer_aggregate_fields {
  avg: Customer_avg_fields
  count(columns: [Customer_select_column!], distinct: Boolean): Int!
  max: Customer_max_fields
  min: Customer_min_fields
  stddev: Customer_stddev_fields
  stddev_pop: Customer_stddev_pop_fields
  stddev_samp: Customer_stddev_samp_fields
  sum: Customer_sum_fields
  var_pop: Customer_var_pop_fields
  var_samp: Customer_var_samp_fields
  variance: Customer_variance_fields
}

"""
order by aggregate values of table "Customer"
"""
input Customer_aggregate_order_by {
  avg: Customer_avg_order_by
  count: order_by
  max: Customer_max_order_by
  min: Customer_min_order_by
  stddev: Customer_stddev_order_by
  stddev_pop: Customer_stddev_pop_order_by
  stddev_samp: Customer_stddev_samp_order_by
  sum: Customer_sum_order_by
  var_pop: Customer_var_pop_order_by
  var_samp: Customer_var_samp_order_by
  variance: Customer_variance_order_by
}

"""
input type for inserting array relation for remote table "Customer"
"""
input Customer_arr_rel_insert_input {
  data: [Customer_insert_input!]!

  """upsert condition"""
  on_conflict: Customer_on_conflict
}

"""aggregate avg on columns"""
type Customer_avg_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by avg() on columns of table "Customer"
"""
input Customer_avg_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""
Boolean expression to filter rows from the table "Customer". All fields are combined with a logical 'AND'.
"""
input Customer_bool_exp {
  Address: String_comparison_exp
  City: String_comparison_exp
  Company: String_comparison_exp
  Country: String_comparison_exp
  CustomerId: Int_comparison_exp
  Email: String_comparison_exp
  Employee: Employee_bool_exp
  Fax: String_comparison_exp
  FirstName: String_comparison_exp
  Invoices: Invoice_bool_exp
  Invoices_aggregate: Invoice_aggregate_bool_exp
  LastName: String_comparison_exp
  Phone: String_comparison_exp
  PostalCode: String_comparison_exp
  State: String_comparison_exp
  SupportRepId: Int_comparison_exp
  _and: [Customer_bool_exp!]
  _not: Customer_bool_exp
  _or: [Customer_bool_exp!]
}

"""
unique or primary key constraints on table "Customer"
"""
enum Customer_constraint {
  """
  unique or primary key constraint on columns "CustomerId"
  """
  PK_Customer
}

"""
input type for incrementing numeric columns in table "Customer"
"""
input Customer_inc_input {
  CustomerId: Int
  SupportRepId: Int
}

"""
input type for inserting data into table "Customer"
"""
input Customer_insert_input {
  Address: String
  City: String
  Company: String
  Country: String
  CustomerId: Int
  Email: String
  Employee: Employee_obj_rel_insert_input
  Fax: String
  FirstName: String
  Invoices: Invoice_arr_rel_insert_input
  LastName: String
  Phone: String
  PostalCode: String
  State: String
  SupportRepId: Int
}

"""aggregate max on columns"""
type Customer_max_fields {
  Address: String
  City: String
  Company: String
  Country: String
  CustomerId: Int
  Email: String
  Fax: String
  FirstName: String
  LastName: String
  Phone: String
  PostalCode: String
  State: String
  SupportRepId: Int
}

"""
order by max() on columns of table "Customer"
"""
input Customer_max_order_by {
  Address: order_by
  City: order_by
  Company: order_by
  Country: order_by
  CustomerId: order_by
  Email: order_by
  Fax: order_by
  FirstName: order_by
  LastName: order_by
  Phone: order_by
  PostalCode: order_by
  State: order_by
  SupportRepId: order_by
}

"""aggregate min on columns"""
type Customer_min_fields {
  Address: String
  City: String
  Company: String
  Country: String
  CustomerId: Int
  Email: String
  Fax: String
  FirstName: String
  LastName: String
  Phone: String
  PostalCode: String
  State: String
  SupportRepId: Int
}

"""
order by min() on columns of table "Customer"
"""
input Customer_min_order_by {
  Address: order_by
  City: order_by
  Company: order_by
  Country: order_by
  CustomerId: order_by
  Email: order_by
  Fax: order_by
  FirstName: order_by
  LastName: order_by
  Phone: order_by
  PostalCode: order_by
  State: order_by
  SupportRepId: order_by
}

"""
response of any mutation on the table "Customer"
"""
type Customer_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Customer!]!
}

"""
input type for inserting object relation for remote table "Customer"
"""
input Customer_obj_rel_insert_input {
  data: Customer_insert_input!

  """upsert condition"""
  on_conflict: Customer_on_conflict
}

"""
on_conflict condition type for table "Customer"
"""
input Customer_on_conflict {
  constraint: Customer_constraint!
  update_columns: [Customer_update_column!]! = []
  where: Customer_bool_exp
}

"""Ordering options when selecting data from "Customer"."""
input Customer_order_by {
  Address: order_by
  City: order_by
  Company: order_by
  Country: order_by
  CustomerId: order_by
  Email: order_by
  Employee: Employee_order_by
  Fax: order_by
  FirstName: order_by
  Invoices_aggregate: Invoice_aggregate_order_by
  LastName: order_by
  Phone: order_by
  PostalCode: order_by
  State: order_by
  SupportRepId: order_by
}

"""primary key columns input for table: Customer"""
input Customer_pk_columns_input {
  CustomerId: Int!
}

"""
select columns of table "Customer"
"""
enum Customer_select_column {
  """column name"""
  Address

  """column name"""
  City

  """column name"""
  Company

  """column name"""
  Country

  """column name"""
  CustomerId

  """column name"""
  Email

  """column name"""
  Fax

  """column name"""
  FirstName

  """column name"""
  LastName

  """column name"""
  Phone

  """column name"""
  PostalCode

  """column name"""
  State

  """column name"""
  SupportRepId
}

"""
input type for updating data in table "Customer"
"""
input Customer_set_input {
  Address: String
  City: String
  Company: String
  Country: String
  CustomerId: Int
  Email: String
  Fax: String
  FirstName: String
  LastName: String
  Phone: String
  PostalCode: String
  State: String
  SupportRepId: Int
}

"""aggregate stddev on columns"""
type Customer_stddev_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by stddev() on columns of table "Customer"
"""
input Customer_stddev_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""aggregate stddev_pop on columns"""
type Customer_stddev_pop_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by stddev_pop() on columns of table "Customer"
"""
input Customer_stddev_pop_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""aggregate stddev_samp on columns"""
type Customer_stddev_samp_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by stddev_samp() on columns of table "Customer"
"""
input Customer_stddev_samp_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""
Streaming cursor of the table "Customer"
"""
input Customer_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Customer_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Customer_stream_cursor_value_input {
  Address: String
  City: String
  Company: String
  Country: String
  CustomerId: Int
  Email: String
  Fax: String
  FirstName: String
  LastName: String
  Phone: String
  PostalCode: String
  State: String
  SupportRepId: Int
}

"""aggregate sum on columns"""
type Customer_sum_fields {
  CustomerId: Int
  SupportRepId: Int
}

"""
order by sum() on columns of table "Customer"
"""
input Customer_sum_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""
update columns of table "Customer"
"""
enum Customer_update_column {
  """column name"""
  Address

  """column name"""
  City

  """column name"""
  Company

  """column name"""
  Country

  """column name"""
  CustomerId

  """column name"""
  Email

  """column name"""
  Fax

  """column name"""
  FirstName

  """column name"""
  LastName

  """column name"""
  Phone

  """column name"""
  PostalCode

  """column name"""
  State

  """column name"""
  SupportRepId
}

input Customer_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Customer_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Customer_set_input

  """filter the rows which have to be updated"""
  where: Customer_bool_exp!
}

"""aggregate var_pop on columns"""
type Customer_var_pop_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by var_pop() on columns of table "Customer"
"""
input Customer_var_pop_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""aggregate var_samp on columns"""
type Customer_var_samp_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by var_samp() on columns of table "Customer"
"""
input Customer_var_samp_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""aggregate variance on columns"""
type Customer_variance_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by variance() on columns of table "Customer"
"""
input Customer_variance_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""
columns and relationships of "Employee"
"""
type Employee {
  Address: String
  BirthDate: timestamp
  City: String
  Country: String

  """An array relationship"""
  Customers(
    """distinct select on columns"""
    distinct_on: [Customer_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Customer_order_by!]

    """filter the rows returned"""
    where: Customer_bool_exp
  ): [Customer!]!

  """An aggregate relationship"""
  Customers_aggregate(
    """distinct select on columns"""
    distinct_on: [Customer_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Customer_order_by!]

    """filter the rows returned"""
    where: Customer_bool_exp
  ): Customer_aggregate!
  Email: String

  """An object relationship"""
  Employee: Employee
  EmployeeId: Int!

  """An array relationship"""
  Employees(
    """distinct select on columns"""
    distinct_on: [Employee_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Employee_order_by!]

    """filter the rows returned"""
    where: Employee_bool_exp
  ): [Employee!]!

  """An aggregate relationship"""
  Employees_aggregate(
    """distinct select on columns"""
    distinct_on: [Employee_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Employee_order_by!]

    """filter the rows returned"""
    where: Employee_bool_exp
  ): Employee_aggregate!
  Fax: String
  FirstName: String!
  HireDate: timestamp
  LastName: String!
  Phone: String
  PostalCode: String
  ReportsTo: Int
  State: String
  Title: String
}

"""
aggregated selection of "Employee"
"""
type Employee_aggregate {
  aggregate: Employee_aggregate_fields
  nodes: [Employee!]!
}

input Employee_aggregate_bool_exp {
  count: Employee_aggregate_bool_exp_count
}

input Employee_aggregate_bool_exp_count {
  arguments: [Employee_select_column!]
  distinct: Boolean
  filter: Employee_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "Employee"
"""
type Employee_aggregate_fields {
  avg: Employee_avg_fields
  count(columns: [Employee_select_column!], distinct: Boolean): Int!
  max: Employee_max_fields
  min: Employee_min_fields
  stddev: Employee_stddev_fields
  stddev_pop: Employee_stddev_pop_fields
  stddev_samp: Employee_stddev_samp_fields
  sum: Employee_sum_fields
  var_pop: Employee_var_pop_fields
  var_samp: Employee_var_samp_fields
  variance: Employee_variance_fields
}

"""
order by aggregate values of table "Employee"
"""
input Employee_aggregate_order_by {
  avg: Employee_avg_order_by
  count: order_by
  max: Employee_max_order_by
  min: Employee_min_order_by
  stddev: Employee_stddev_order_by
  stddev_pop: Employee_stddev_pop_order_by
  stddev_samp: Employee_stddev_samp_order_by
  sum: Employee_sum_order_by
  var_pop: Employee_var_pop_order_by
  var_samp: Employee_var_samp_order_by
  variance: Employee_variance_order_by
}

"""
input type for inserting array relation for remote table "Employee"
"""
input Employee_arr_rel_insert_input {
  data: [Employee_insert_input!]!

  """upsert condition"""
  on_conflict: Employee_on_conflict
}

"""aggregate avg on columns"""
type Employee_avg_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by avg() on columns of table "Employee"
"""
input Employee_avg_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""
Boolean expression to filter rows from the table "Employee". All fields are combined with a logical 'AND'.
"""
input Employee_bool_exp {
  Address: String_comparison_exp
  BirthDate: timestamp_comparison_exp
  City: String_comparison_exp
  Country: String_comparison_exp
  Customers: Customer_bool_exp
  Customers_aggregate: Customer_aggregate_bool_exp
  Email: String_comparison_exp
  Employee: Employee_bool_exp
  EmployeeId: Int_comparison_exp
  Employees: Employee_bool_exp
  Employees_aggregate: Employee_aggregate_bool_exp
  Fax: String_comparison_exp
  FirstName: String_comparison_exp
  HireDate: timestamp_comparison_exp
  LastName: String_comparison_exp
  Phone: String_comparison_exp
  PostalCode: String_comparison_exp
  ReportsTo: Int_comparison_exp
  State: String_comparison_exp
  Title: String_comparison_exp
  _and: [Employee_bool_exp!]
  _not: Employee_bool_exp
  _or: [Employee_bool_exp!]
}

"""
unique or primary key constraints on table "Employee"
"""
enum Employee_constraint {
  """
  unique or primary key constraint on columns "EmployeeId"
  """
  PK_Employee
}

"""
input type for incrementing numeric columns in table "Employee"
"""
input Employee_inc_input {
  EmployeeId: Int
  ReportsTo: Int
}

"""
input type for inserting data into table "Employee"
"""
input Employee_insert_input {
  Address: String
  BirthDate: timestamp
  City: String
  Country: String
  Customers: Customer_arr_rel_insert_input
  Email: String
  Employee: Employee_obj_rel_insert_input
  EmployeeId: Int
  Employees: Employee_arr_rel_insert_input
  Fax: String
  FirstName: String
  HireDate: timestamp
  LastName: String
  Phone: String
  PostalCode: String
  ReportsTo: Int
  State: String
  Title: String
}

"""aggregate max on columns"""
type Employee_max_fields {
  Address: String
  BirthDate: timestamp
  City: String
  Country: String
  Email: String
  EmployeeId: Int
  Fax: String
  FirstName: String
  HireDate: timestamp
  LastName: String
  Phone: String
  PostalCode: String
  ReportsTo: Int
  State: String
  Title: String
}

"""
order by max() on columns of table "Employee"
"""
input Employee_max_order_by {
  Address: order_by
  BirthDate: order_by
  City: order_by
  Country: order_by
  Email: order_by
  EmployeeId: order_by
  Fax: order_by
  FirstName: order_by
  HireDate: order_by
  LastName: order_by
  Phone: order_by
  PostalCode: order_by
  ReportsTo: order_by
  State: order_by
  Title: order_by
}

"""aggregate min on columns"""
type Employee_min_fields {
  Address: String
  BirthDate: timestamp
  City: String
  Country: String
  Email: String
  EmployeeId: Int
  Fax: String
  FirstName: String
  HireDate: timestamp
  LastName: String
  Phone: String
  PostalCode: String
  ReportsTo: Int
  State: String
  Title: String
}

"""
order by min() on columns of table "Employee"
"""
input Employee_min_order_by {
  Address: order_by
  BirthDate: order_by
  City: order_by
  Country: order_by
  Email: order_by
  EmployeeId: order_by
  Fax: order_by
  FirstName: order_by
  HireDate: order_by
  LastName: order_by
  Phone: order_by
  PostalCode: order_by
  ReportsTo: order_by
  State: order_by
  Title: order_by
}

"""
response of any mutation on the table "Employee"
"""
type Employee_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Employee!]!
}

"""
input type for inserting object relation for remote table "Employee"
"""
input Employee_obj_rel_insert_input {
  data: Employee_insert_input!

  """upsert condition"""
  on_conflict: Employee_on_conflict
}

"""
on_conflict condition type for table "Employee"
"""
input Employee_on_conflict {
  constraint: Employee_constraint!
  update_columns: [Employee_update_column!]! = []
  where: Employee_bool_exp
}

"""Ordering options when selecting data from "Employee"."""
input Employee_order_by {
  Address: order_by
  BirthDate: order_by
  City: order_by
  Country: order_by
  Customers_aggregate: Customer_aggregate_order_by
  Email: order_by
  Employee: Employee_order_by
  EmployeeId: order_by
  Employees_aggregate: Employee_aggregate_order_by
  Fax: order_by
  FirstName: order_by
  HireDate: order_by
  LastName: order_by
  Phone: order_by
  PostalCode: order_by
  ReportsTo: order_by
  State: order_by
  Title: order_by
}

"""primary key columns input for table: Employee"""
input Employee_pk_columns_input {
  EmployeeId: Int!
}

"""
select columns of table "Employee"
"""
enum Employee_select_column {
  """column name"""
  Address

  """column name"""
  BirthDate

  """column name"""
  City

  """column name"""
  Country

  """column name"""
  Email

  """column name"""
  EmployeeId

  """column name"""
  Fax

  """column name"""
  FirstName

  """column name"""
  HireDate

  """column name"""
  LastName

  """column name"""
  Phone

  """column name"""
  PostalCode

  """column name"""
  ReportsTo

  """column name"""
  State

  """column name"""
  Title
}

"""
input type for updating data in table "Employee"
"""
input Employee_set_input {
  Address: String
  BirthDate: timestamp
  City: String
  Country: String
  Email: String
  EmployeeId: Int
  Fax: String
  FirstName: String
  HireDate: timestamp
  LastName: String
  Phone: String
  PostalCode: String
  ReportsTo: Int
  State: String
  Title: String
}

"""aggregate stddev on columns"""
type Employee_stddev_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by stddev() on columns of table "Employee"
"""
input Employee_stddev_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""aggregate stddev_pop on columns"""
type Employee_stddev_pop_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by stddev_pop() on columns of table "Employee"
"""
input Employee_stddev_pop_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""aggregate stddev_samp on columns"""
type Employee_stddev_samp_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by stddev_samp() on columns of table "Employee"
"""
input Employee_stddev_samp_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""
Streaming cursor of the table "Employee"
"""
input Employee_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Employee_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Employee_stream_cursor_value_input {
  Address: String
  BirthDate: timestamp
  City: String
  Country: String
  Email: String
  EmployeeId: Int
  Fax: String
  FirstName: String
  HireDate: timestamp
  LastName: String
  Phone: String
  PostalCode: String
  ReportsTo: Int
  State: String
  Title: String
}

"""aggregate sum on columns"""
type Employee_sum_fields {
  EmployeeId: Int
  ReportsTo: Int
}

"""
order by sum() on columns of table "Employee"
"""
input Employee_sum_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""
update columns of table "Employee"
"""
enum Employee_update_column {
  """column name"""
  Address

  """column name"""
  BirthDate

  """column name"""
  City

  """column name"""
  Country

  """column name"""
  Email

  """column name"""
  EmployeeId

  """column name"""
  Fax

  """column name"""
  FirstName

  """column name"""
  HireDate

  """column name"""
  LastName

  """column name"""
  Phone

  """column name"""
  PostalCode

  """column name"""
  ReportsTo

  """column name"""
  State

  """column name"""
  Title
}

input Employee_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Employee_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Employee_set_input

  """filter the rows which have to be updated"""
  where: Employee_bool_exp!
}

"""aggregate var_pop on columns"""
type Employee_var_pop_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by var_pop() on columns of table "Employee"
"""
input Employee_var_pop_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""aggregate var_samp on columns"""
type Employee_var_samp_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by var_samp() on columns of table "Employee"
"""
input Employee_var_samp_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""aggregate variance on columns"""
type Employee_variance_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by variance() on columns of table "Employee"
"""
input Employee_variance_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""
columns and relationships of "Genre"
"""
type Genre {
  GenreId: Int!
  Name: String

  """An array relationship"""
  Tracks(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): [Track!]!

  """An aggregate relationship"""
  Tracks_aggregate(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): Track_aggregate!
}

"""
aggregated selection of "Genre"
"""
type Genre_aggregate {
  aggregate: Genre_aggregate_fields
  nodes: [Genre!]!
}

"""
aggregate fields of "Genre"
"""
type Genre_aggregate_fields {
  avg: Genre_avg_fields
  count(columns: [Genre_select_column!], distinct: Boolean): Int!
  max: Genre_max_fields
  min: Genre_min_fields
  stddev: Genre_stddev_fields
  stddev_pop: Genre_stddev_pop_fields
  stddev_samp: Genre_stddev_samp_fields
  sum: Genre_sum_fields
  var_pop: Genre_var_pop_fields
  var_samp: Genre_var_samp_fields
  variance: Genre_variance_fields
}

"""aggregate avg on columns"""
type Genre_avg_fields {
  GenreId: Float
}

"""
Boolean expression to filter rows from the table "Genre". All fields are combined with a logical 'AND'.
"""
input Genre_bool_exp {
  GenreId: Int_comparison_exp
  Name: String_comparison_exp
  Tracks: Track_bool_exp
  Tracks_aggregate: Track_aggregate_bool_exp
  _and: [Genre_bool_exp!]
  _not: Genre_bool_exp
  _or: [Genre_bool_exp!]
}

"""
unique or primary key constraints on table "Genre"
"""
enum Genre_constraint {
  """
  unique or primary key constraint on columns "GenreId"
  """
  PK_Genre
}

"""
input type for incrementing numeric columns in table "Genre"
"""
input Genre_inc_input {
  GenreId: Int
}

"""
input type for inserting data into table "Genre"
"""
input Genre_insert_input {
  GenreId: Int
  Name: String
  Tracks: Track_arr_rel_insert_input
}

"""aggregate max on columns"""
type Genre_max_fields {
  GenreId: Int
  Name: String
}

"""aggregate min on columns"""
type Genre_min_fields {
  GenreId: Int
  Name: String
}

"""
response of any mutation on the table "Genre"
"""
type Genre_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Genre!]!
}

"""
input type for inserting object relation for remote table "Genre"
"""
input Genre_obj_rel_insert_input {
  data: Genre_insert_input!

  """upsert condition"""
  on_conflict: Genre_on_conflict
}

"""
on_conflict condition type for table "Genre"
"""
input Genre_on_conflict {
  constraint: Genre_constraint!
  update_columns: [Genre_update_column!]! = []
  where: Genre_bool_exp
}

"""Ordering options when selecting data from "Genre"."""
input Genre_order_by {
  GenreId: order_by
  Name: order_by
  Tracks_aggregate: Track_aggregate_order_by
}

"""primary key columns input for table: Genre"""
input Genre_pk_columns_input {
  GenreId: Int!
}

"""
select columns of table "Genre"
"""
enum Genre_select_column {
  """column name"""
  GenreId

  """column name"""
  Name
}

"""
input type for updating data in table "Genre"
"""
input Genre_set_input {
  GenreId: Int
  Name: String
}

"""aggregate stddev on columns"""
type Genre_stddev_fields {
  GenreId: Float
}

"""aggregate stddev_pop on columns"""
type Genre_stddev_pop_fields {
  GenreId: Float
}

"""aggregate stddev_samp on columns"""
type Genre_stddev_samp_fields {
  GenreId: Float
}

"""
Streaming cursor of the table "Genre"
"""
input Genre_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Genre_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Genre_stream_cursor_value_input {
  GenreId: Int
  Name: String
}

"""aggregate sum on columns"""
type Genre_sum_fields {
  GenreId: Int
}

"""
update columns of table "Genre"
"""
enum Genre_update_column {
  """column name"""
  GenreId

  """column name"""
  Name
}

input Genre_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Genre_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Genre_set_input

  """filter the rows which have to be updated"""
  where: Genre_bool_exp!
}

"""aggregate var_pop on columns"""
type Genre_var_pop_fields {
  GenreId: Float
}

"""aggregate var_samp on columns"""
type Genre_var_samp_fields {
  GenreId: Float
}

"""aggregate variance on columns"""
type Genre_variance_fields {
  GenreId: Float
}

"""
Boolean expression to compare columns of type "Int". All fields are combined with logical 'AND'.
"""
input Int_comparison_exp {
  _eq: Int
  _gt: Int
  _gte: Int
  _in: [Int!]
  _is_null: Boolean
  _lt: Int
  _lte: Int
  _neq: Int
  _nin: [Int!]
}

"""
columns and relationships of "Invoice"
"""
type Invoice {
  BillingAddress: String
  BillingCity: String
  BillingCountry: String
  BillingPostalCode: String
  BillingState: String

  """An object relationship"""
  Customer: Customer!
  CustomerId: Int!
  InvoiceDate: timestamp!
  InvoiceId: Int!

  """An array relationship"""
  InvoiceLines(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): [InvoiceLine!]!

  """An aggregate relationship"""
  InvoiceLines_aggregate(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): InvoiceLine_aggregate!
  Total: numeric!
}

"""
columns and relationships of "InvoiceLine"
"""
type InvoiceLine {
  """An object relationship"""
  Invoice: Invoice!
  InvoiceId: Int!
  InvoiceLineId: Int!
  Quantity: Int!

  """An object relationship"""
  Track: Track!
  TrackId: Int!
  UnitPrice: numeric!
}

"""
aggregated selection of "InvoiceLine"
"""
type InvoiceLine_aggregate {
  aggregate: InvoiceLine_aggregate_fields
  nodes: [InvoiceLine!]!
}

input InvoiceLine_aggregate_bool_exp {
  count: InvoiceLine_aggregate_bool_exp_count
}

input InvoiceLine_aggregate_bool_exp_count {
  arguments: [InvoiceLine_select_column!]
  distinct: Boolean
  filter: InvoiceLine_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "InvoiceLine"
"""
type InvoiceLine_aggregate_fields {
  avg: InvoiceLine_avg_fields
  count(columns: [InvoiceLine_select_column!], distinct: Boolean): Int!
  max: InvoiceLine_max_fields
  min: InvoiceLine_min_fields
  stddev: InvoiceLine_stddev_fields
  stddev_pop: InvoiceLine_stddev_pop_fields
  stddev_samp: InvoiceLine_stddev_samp_fields
  sum: InvoiceLine_sum_fields
  var_pop: InvoiceLine_var_pop_fields
  var_samp: InvoiceLine_var_samp_fields
  variance: InvoiceLine_variance_fields
}

"""
order by aggregate values of table "InvoiceLine"
"""
input InvoiceLine_aggregate_order_by {
  avg: InvoiceLine_avg_order_by
  count: order_by
  max: InvoiceLine_max_order_by
  min: InvoiceLine_min_order_by
  stddev: InvoiceLine_stddev_order_by
  stddev_pop: InvoiceLine_stddev_pop_order_by
  stddev_samp: InvoiceLine_stddev_samp_order_by
  sum: InvoiceLine_sum_order_by
  var_pop: InvoiceLine_var_pop_order_by
  var_samp: InvoiceLine_var_samp_order_by
  variance: InvoiceLine_variance_order_by
}

"""
input type for inserting array relation for remote table "InvoiceLine"
"""
input InvoiceLine_arr_rel_insert_input {
  data: [InvoiceLine_insert_input!]!

  """upsert condition"""
  on_conflict: InvoiceLine_on_conflict
}

"""aggregate avg on columns"""
type InvoiceLine_avg_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by avg() on columns of table "InvoiceLine"
"""
input InvoiceLine_avg_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
Boolean expression to filter rows from the table "InvoiceLine". All fields are combined with a logical 'AND'.
"""
input InvoiceLine_bool_exp {
  Invoice: Invoice_bool_exp
  InvoiceId: Int_comparison_exp
  InvoiceLineId: Int_comparison_exp
  Quantity: Int_comparison_exp
  Track: Track_bool_exp
  TrackId: Int_comparison_exp
  UnitPrice: numeric_comparison_exp
  _and: [InvoiceLine_bool_exp!]
  _not: InvoiceLine_bool_exp
  _or: [InvoiceLine_bool_exp!]
}

"""
unique or primary key constraints on table "InvoiceLine"
"""
enum InvoiceLine_constraint {
  """
  unique or primary key constraint on columns "InvoiceLineId"
  """
  PK_InvoiceLine
}

"""
input type for incrementing numeric columns in table "InvoiceLine"
"""
input InvoiceLine_inc_input {
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  TrackId: Int
  UnitPrice: numeric
}

"""
input type for inserting data into table "InvoiceLine"
"""
input InvoiceLine_insert_input {
  Invoice: Invoice_obj_rel_insert_input
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  Track: Track_obj_rel_insert_input
  TrackId: Int
  UnitPrice: numeric
}

"""aggregate max on columns"""
type InvoiceLine_max_fields {
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  TrackId: Int
  UnitPrice: numeric
}

"""
order by max() on columns of table "InvoiceLine"
"""
input InvoiceLine_max_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate min on columns"""
type InvoiceLine_min_fields {
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  TrackId: Int
  UnitPrice: numeric
}

"""
order by min() on columns of table "InvoiceLine"
"""
input InvoiceLine_min_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
response of any mutation on the table "InvoiceLine"
"""
type InvoiceLine_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [InvoiceLine!]!
}

"""
on_conflict condition type for table "InvoiceLine"
"""
input InvoiceLine_on_conflict {
  constraint: InvoiceLine_constraint!
  update_columns: [InvoiceLine_update_column!]! = []
  where: InvoiceLine_bool_exp
}

"""Ordering options when selecting data from "InvoiceLine"."""
input InvoiceLine_order_by {
  Invoice: Invoice_order_by
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  Track: Track_order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""primary key columns input for table: InvoiceLine"""
input InvoiceLine_pk_columns_input {
  InvoiceLineId: Int!
}

"""
select columns of table "InvoiceLine"
"""
enum InvoiceLine_select_column {
  """column name"""
  InvoiceId

  """column name"""
  InvoiceLineId

  """column name"""
  Quantity

  """column name"""
  TrackId

  """column name"""
  UnitPrice
}

"""
input type for updating data in table "InvoiceLine"
"""
input InvoiceLine_set_input {
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  TrackId: Int
  UnitPrice: numeric
}

"""aggregate stddev on columns"""
type InvoiceLine_stddev_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by stddev() on columns of table "InvoiceLine"
"""
input InvoiceLine_stddev_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate stddev_pop on columns"""
type InvoiceLine_stddev_pop_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by stddev_pop() on columns of table "InvoiceLine"
"""
input InvoiceLine_stddev_pop_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate stddev_samp on columns"""
type InvoiceLine_stddev_samp_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by stddev_samp() on columns of table "InvoiceLine"
"""
input InvoiceLine_stddev_samp_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
Streaming cursor of the table "InvoiceLine"
"""
input InvoiceLine_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: InvoiceLine_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input InvoiceLine_stream_cursor_value_input {
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  TrackId: Int
  UnitPrice: numeric
}

"""aggregate sum on columns"""
type InvoiceLine_sum_fields {
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  TrackId: Int
  UnitPrice: numeric
}

"""
order by sum() on columns of table "InvoiceLine"
"""
input InvoiceLine_sum_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
update columns of table "InvoiceLine"
"""
enum InvoiceLine_update_column {
  """column name"""
  InvoiceId

  """column name"""
  InvoiceLineId

  """column name"""
  Quantity

  """column name"""
  TrackId

  """column name"""
  UnitPrice
}

input InvoiceLine_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: InvoiceLine_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: InvoiceLine_set_input

  """filter the rows which have to be updated"""
  where: InvoiceLine_bool_exp!
}

"""aggregate var_pop on columns"""
type InvoiceLine_var_pop_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by var_pop() on columns of table "InvoiceLine"
"""
input InvoiceLine_var_pop_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate var_samp on columns"""
type InvoiceLine_var_samp_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by var_samp() on columns of table "InvoiceLine"
"""
input InvoiceLine_var_samp_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate variance on columns"""
type InvoiceLine_variance_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by variance() on columns of table "InvoiceLine"
"""
input InvoiceLine_variance_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
aggregated selection of "Invoice"
"""
type Invoice_aggregate {
  aggregate: Invoice_aggregate_fields
  nodes: [Invoice!]!
}

input Invoice_aggregate_bool_exp {
  count: Invoice_aggregate_bool_exp_count
}

input Invoice_aggregate_bool_exp_count {
  arguments: [Invoice_select_column!]
  distinct: Boolean
  filter: Invoice_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "Invoice"
"""
type Invoice_aggregate_fields {
  avg: Invoice_avg_fields
  count(columns: [Invoice_select_column!], distinct: Boolean): Int!
  max: Invoice_max_fields
  min: Invoice_min_fields
  stddev: Invoice_stddev_fields
  stddev_pop: Invoice_stddev_pop_fields
  stddev_samp: Invoice_stddev_samp_fields
  sum: Invoice_sum_fields
  var_pop: Invoice_var_pop_fields
  var_samp: Invoice_var_samp_fields
  variance: Invoice_variance_fields
}

"""
order by aggregate values of table "Invoice"
"""
input Invoice_aggregate_order_by {
  avg: Invoice_avg_order_by
  count: order_by
  max: Invoice_max_order_by
  min: Invoice_min_order_by
  stddev: Invoice_stddev_order_by
  stddev_pop: Invoice_stddev_pop_order_by
  stddev_samp: Invoice_stddev_samp_order_by
  sum: Invoice_sum_order_by
  var_pop: Invoice_var_pop_order_by
  var_samp: Invoice_var_samp_order_by
  variance: Invoice_variance_order_by
}

"""
input type for inserting array relation for remote table "Invoice"
"""
input Invoice_arr_rel_insert_input {
  data: [Invoice_insert_input!]!

  """upsert condition"""
  on_conflict: Invoice_on_conflict
}

"""aggregate avg on columns"""
type Invoice_avg_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by avg() on columns of table "Invoice"
"""
input Invoice_avg_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""
Boolean expression to filter rows from the table "Invoice". All fields are combined with a logical 'AND'.
"""
input Invoice_bool_exp {
  BillingAddress: String_comparison_exp
  BillingCity: String_comparison_exp
  BillingCountry: String_comparison_exp
  BillingPostalCode: String_comparison_exp
  BillingState: String_comparison_exp
  Customer: Customer_bool_exp
  CustomerId: Int_comparison_exp
  InvoiceDate: timestamp_comparison_exp
  InvoiceId: Int_comparison_exp
  InvoiceLines: InvoiceLine_bool_exp
  InvoiceLines_aggregate: InvoiceLine_aggregate_bool_exp
  Total: numeric_comparison_exp
  _and: [Invoice_bool_exp!]
  _not: Invoice_bool_exp
  _or: [Invoice_bool_exp!]
}

"""
unique or primary key constraints on table "Invoice"
"""
enum Invoice_constraint {
  """
  unique or primary key constraint on columns "InvoiceId"
  """
  PK_Invoice
}

"""
input type for incrementing numeric columns in table "Invoice"
"""
input Invoice_inc_input {
  CustomerId: Int
  InvoiceId: Int
  Total: numeric
}

"""
input type for inserting data into table "Invoice"
"""
input Invoice_insert_input {
  BillingAddress: String
  BillingCity: String
  BillingCountry: String
  BillingPostalCode: String
  BillingState: String
  Customer: Customer_obj_rel_insert_input
  CustomerId: Int
  InvoiceDate: timestamp
  InvoiceId: Int
  InvoiceLines: InvoiceLine_arr_rel_insert_input
  Total: numeric
}

"""aggregate max on columns"""
type Invoice_max_fields {
  BillingAddress: String
  BillingCity: String
  BillingCountry: String
  BillingPostalCode: String
  BillingState: String
  CustomerId: Int
  InvoiceDate: timestamp
  InvoiceId: Int
  Total: numeric
}

"""
order by max() on columns of table "Invoice"
"""
input Invoice_max_order_by {
  BillingAddress: order_by
  BillingCity: order_by
  BillingCountry: order_by
  BillingPostalCode: order_by
  BillingState: order_by
  CustomerId: order_by
  InvoiceDate: order_by
  InvoiceId: order_by
  Total: order_by
}

"""aggregate min on columns"""
type Invoice_min_fields {
  BillingAddress: String
  BillingCity: String
  BillingCountry: String
  BillingPostalCode: String
  BillingState: String
  CustomerId: Int
  InvoiceDate: timestamp
  InvoiceId: Int
  Total: numeric
}

"""
order by min() on columns of table "Invoice"
"""
input Invoice_min_order_by {
  BillingAddress: order_by
  BillingCity: order_by
  BillingCountry: order_by
  BillingPostalCode: order_by
  BillingState: order_by
  CustomerId: order_by
  InvoiceDate: order_by
  InvoiceId: order_by
  Total: order_by
}

"""
response of any mutation on the table "Invoice"
"""
type Invoice_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Invoice!]!
}

"""
input type for inserting object relation for remote table "Invoice"
"""
input Invoice_obj_rel_insert_input {
  data: Invoice_insert_input!

  """upsert condition"""
  on_conflict: Invoice_on_conflict
}

"""
on_conflict condition type for table "Invoice"
"""
input Invoice_on_conflict {
  constraint: Invoice_constraint!
  update_columns: [Invoice_update_column!]! = []
  where: Invoice_bool_exp
}

"""Ordering options when selecting data from "Invoice"."""
input Invoice_order_by {
  BillingAddress: order_by
  BillingCity: order_by
  BillingCountry: order_by
  BillingPostalCode: order_by
  BillingState: order_by
  Customer: Customer_order_by
  CustomerId: order_by
  InvoiceDate: order_by
  InvoiceId: order_by
  InvoiceLines_aggregate: InvoiceLine_aggregate_order_by
  Total: order_by
}

"""primary key columns input for table: Invoice"""
input Invoice_pk_columns_input {
  InvoiceId: Int!
}

"""
select columns of table "Invoice"
"""
enum Invoice_select_column {
  """column name"""
  BillingAddress

  """column name"""
  BillingCity

  """column name"""
  BillingCountry

  """column name"""
  BillingPostalCode

  """column name"""
  BillingState

  """column name"""
  CustomerId

  """column name"""
  InvoiceDate

  """column name"""
  InvoiceId

  """column name"""
  Total
}

"""
input type for updating data in table "Invoice"
"""
input Invoice_set_input {
  BillingAddress: String
  BillingCity: String
  BillingCountry: String
  BillingPostalCode: String
  BillingState: String
  CustomerId: Int
  InvoiceDate: timestamp
  InvoiceId: Int
  Total: numeric
}

"""aggregate stddev on columns"""
type Invoice_stddev_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by stddev() on columns of table "Invoice"
"""
input Invoice_stddev_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""aggregate stddev_pop on columns"""
type Invoice_stddev_pop_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by stddev_pop() on columns of table "Invoice"
"""
input Invoice_stddev_pop_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""aggregate stddev_samp on columns"""
type Invoice_stddev_samp_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by stddev_samp() on columns of table "Invoice"
"""
input Invoice_stddev_samp_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""
Streaming cursor of the table "Invoice"
"""
input Invoice_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Invoice_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Invoice_stream_cursor_value_input {
  BillingAddress: String
  BillingCity: String
  BillingCountry: String
  BillingPostalCode: String
  BillingState: String
  CustomerId: Int
  InvoiceDate: timestamp
  InvoiceId: Int
  Total: numeric
}

"""aggregate sum on columns"""
type Invoice_sum_fields {
  CustomerId: Int
  InvoiceId: Int
  Total: numeric
}

"""
order by sum() on columns of table "Invoice"
"""
input Invoice_sum_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""
update columns of table "Invoice"
"""
enum Invoice_update_column {
  """column name"""
  BillingAddress

  """column name"""
  BillingCity

  """column name"""
  BillingCountry

  """column name"""
  BillingPostalCode

  """column name"""
  BillingState

  """column name"""
  CustomerId

  """column name"""
  InvoiceDate

  """column name"""
  InvoiceId

  """column name"""
  Total
}

input Invoice_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Invoice_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Invoice_set_input

  """filter the rows which have to be updated"""
  where: Invoice_bool_exp!
}

"""aggregate var_pop on columns"""
type Invoice_var_pop_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by var_pop() on columns of table "Invoice"
"""
input Invoice_var_pop_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""aggregate var_samp on columns"""
type Invoice_var_samp_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by var_samp() on columns of table "Invoice"
"""
input Invoice_var_samp_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""aggregate variance on columns"""
type Invoice_variance_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by variance() on columns of table "Invoice"
"""
input Invoice_variance_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""
columns and relationships of "MediaType"
"""
type MediaType {
  MediaTypeId: Int!
  Name: String

  """An array relationship"""
  Tracks(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): [Track!]!

  """An aggregate relationship"""
  Tracks_aggregate(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): Track_aggregate!
}

"""
aggregated selection of "MediaType"
"""
type MediaType_aggregate {
  aggregate: MediaType_aggregate_fields
  nodes: [MediaType!]!
}

"""
aggregate fields of "MediaType"
"""
type MediaType_aggregate_fields {
  avg: MediaType_avg_fields
  count(columns: [MediaType_select_column!], distinct: Boolean): Int!
  max: MediaType_max_fields
  min: MediaType_min_fields
  stddev: MediaType_stddev_fields
  stddev_pop: MediaType_stddev_pop_fields
  stddev_samp: MediaType_stddev_samp_fields
  sum: MediaType_sum_fields
  var_pop: MediaType_var_pop_fields
  var_samp: MediaType_var_samp_fields
  variance: MediaType_variance_fields
}

"""aggregate avg on columns"""
type MediaType_avg_fields {
  MediaTypeId: Float
}

"""
Boolean expression to filter rows from the table "MediaType". All fields are combined with a logical 'AND'.
"""
input MediaType_bool_exp {
  MediaTypeId: Int_comparison_exp
  Name: String_comparison_exp
  Tracks: Track_bool_exp
  Tracks_aggregate: Track_aggregate_bool_exp
  _and: [MediaType_bool_exp!]
  _not: MediaType_bool_exp
  _or: [MediaType_bool_exp!]
}

"""
unique or primary key constraints on table "MediaType"
"""
enum MediaType_constraint {
  """
  unique or primary key constraint on columns "MediaTypeId"
  """
  PK_MediaType
}

"""
input type for incrementing numeric columns in table "MediaType"
"""
input MediaType_inc_input {
  MediaTypeId: Int
}

"""
input type for inserting data into table "MediaType"
"""
input MediaType_insert_input {
  MediaTypeId: Int
  Name: String
  Tracks: Track_arr_rel_insert_input
}

"""aggregate max on columns"""
type MediaType_max_fields {
  MediaTypeId: Int
  Name: String
}

"""aggregate min on columns"""
type MediaType_min_fields {
  MediaTypeId: Int
  Name: String
}

"""
response of any mutation on the table "MediaType"
"""
type MediaType_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [MediaType!]!
}

"""
input type for inserting object relation for remote table "MediaType"
"""
input MediaType_obj_rel_insert_input {
  data: MediaType_insert_input!

  """upsert condition"""
  on_conflict: MediaType_on_conflict
}

"""
on_conflict condition type for table "MediaType"
"""
input MediaType_on_conflict {
  constraint: MediaType_constraint!
  update_columns: [MediaType_update_column!]! = []
  where: MediaType_bool_exp
}

"""Ordering options when selecting data from "MediaType"."""
input MediaType_order_by {
  MediaTypeId: order_by
  Name: order_by
  Tracks_aggregate: Track_aggregate_order_by
}

"""primary key columns input for table: MediaType"""
input MediaType_pk_columns_input {
  MediaTypeId: Int!
}

"""
select columns of table "MediaType"
"""
enum MediaType_select_column {
  """column name"""
  MediaTypeId

  """column name"""
  Name
}

"""
input type for updating data in table "MediaType"
"""
input MediaType_set_input {
  MediaTypeId: Int
  Name: String
}

"""aggregate stddev on columns"""
type MediaType_stddev_fields {
  MediaTypeId: Float
}

"""aggregate stddev_pop on columns"""
type MediaType_stddev_pop_fields {
  MediaTypeId: Float
}

"""aggregate stddev_samp on columns"""
type MediaType_stddev_samp_fields {
  MediaTypeId: Float
}

"""
Streaming cursor of the table "MediaType"
"""
input MediaType_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: MediaType_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input MediaType_stream_cursor_value_input {
  MediaTypeId: Int
  Name: String
}

"""aggregate sum on columns"""
type MediaType_sum_fields {
  MediaTypeId: Int
}

"""
update columns of table "MediaType"
"""
enum MediaType_update_column {
  """column name"""
  MediaTypeId

  """column name"""
  Name
}

input MediaType_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: MediaType_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: MediaType_set_input

  """filter the rows which have to be updated"""
  where: MediaType_bool_exp!
}

"""aggregate var_pop on columns"""
type MediaType_var_pop_fields {
  MediaTypeId: Float
}

"""aggregate var_samp on columns"""
type MediaType_var_samp_fields {
  MediaTypeId: Float
}

"""aggregate variance on columns"""
type MediaType_variance_fields {
  MediaTypeId: Float
}

"""
columns and relationships of "Playlist"
"""
type Playlist {
  Name: String
  PlaylistId: Int!

  """An array relationship"""
  PlaylistTracks(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): [PlaylistTrack!]!

  """An aggregate relationship"""
  PlaylistTracks_aggregate(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): PlaylistTrack_aggregate!
}

"""
columns and relationships of "PlaylistTrack"
"""
type PlaylistTrack {
  """An object relationship"""
  Playlist: Playlist!
  PlaylistId: Int!

  """An object relationship"""
  Track: Track!
  TrackId: Int!
}

"""
aggregated selection of "PlaylistTrack"
"""
type PlaylistTrack_aggregate {
  aggregate: PlaylistTrack_aggregate_fields
  nodes: [PlaylistTrack!]!
}

input PlaylistTrack_aggregate_bool_exp {
  count: PlaylistTrack_aggregate_bool_exp_count
}

input PlaylistTrack_aggregate_bool_exp_count {
  arguments: [PlaylistTrack_select_column!]
  distinct: Boolean
  filter: PlaylistTrack_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "PlaylistTrack"
"""
type PlaylistTrack_aggregate_fields {
  avg: PlaylistTrack_avg_fields
  count(columns: [PlaylistTrack_select_column!], distinct: Boolean): Int!
  max: PlaylistTrack_max_fields
  min: PlaylistTrack_min_fields
  stddev: PlaylistTrack_stddev_fields
  stddev_pop: PlaylistTrack_stddev_pop_fields
  stddev_samp: PlaylistTrack_stddev_samp_fields
  sum: PlaylistTrack_sum_fields
  var_pop: PlaylistTrack_var_pop_fields
  var_samp: PlaylistTrack_var_samp_fields
  variance: PlaylistTrack_variance_fields
}

"""
order by aggregate values of table "PlaylistTrack"
"""
input PlaylistTrack_aggregate_order_by {
  avg: PlaylistTrack_avg_order_by
  count: order_by
  max: PlaylistTrack_max_order_by
  min: PlaylistTrack_min_order_by
  stddev: PlaylistTrack_stddev_order_by
  stddev_pop: PlaylistTrack_stddev_pop_order_by
  stddev_samp: PlaylistTrack_stddev_samp_order_by
  sum: PlaylistTrack_sum_order_by
  var_pop: PlaylistTrack_var_pop_order_by
  var_samp: PlaylistTrack_var_samp_order_by
  variance: PlaylistTrack_variance_order_by
}

"""
input type for inserting array relation for remote table "PlaylistTrack"
"""
input PlaylistTrack_arr_rel_insert_input {
  data: [PlaylistTrack_insert_input!]!

  """upsert condition"""
  on_conflict: PlaylistTrack_on_conflict
}

"""aggregate avg on columns"""
type PlaylistTrack_avg_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by avg() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_avg_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""
Boolean expression to filter rows from the table "PlaylistTrack". All fields are combined with a logical 'AND'.
"""
input PlaylistTrack_bool_exp {
  Playlist: Playlist_bool_exp
  PlaylistId: Int_comparison_exp
  Track: Track_bool_exp
  TrackId: Int_comparison_exp
  _and: [PlaylistTrack_bool_exp!]
  _not: PlaylistTrack_bool_exp
  _or: [PlaylistTrack_bool_exp!]
}

"""
unique or primary key constraints on table "PlaylistTrack"
"""
enum PlaylistTrack_constraint {
  """
  unique or primary key constraint on columns "PlaylistId", "TrackId"
  """
  PK_PlaylistTrack
}

"""
input type for incrementing numeric columns in table "PlaylistTrack"
"""
input PlaylistTrack_inc_input {
  PlaylistId: Int
  TrackId: Int
}

"""
input type for inserting data into table "PlaylistTrack"
"""
input PlaylistTrack_insert_input {
  Playlist: Playlist_obj_rel_insert_input
  PlaylistId: Int
  Track: Track_obj_rel_insert_input
  TrackId: Int
}

"""aggregate max on columns"""
type PlaylistTrack_max_fields {
  PlaylistId: Int
  TrackId: Int
}

"""
order by max() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_max_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""aggregate min on columns"""
type PlaylistTrack_min_fields {
  PlaylistId: Int
  TrackId: Int
}

"""
order by min() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_min_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""
response of any mutation on the table "PlaylistTrack"
"""
type PlaylistTrack_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [PlaylistTrack!]!
}

"""
on_conflict condition type for table "PlaylistTrack"
"""
input PlaylistTrack_on_conflict {
  constraint: PlaylistTrack_constraint!
  update_columns: [PlaylistTrack_update_column!]! = []
  where: PlaylistTrack_bool_exp
}

"""Ordering options when selecting data from "PlaylistTrack"."""
input PlaylistTrack_order_by {
  Playlist: Playlist_order_by
  PlaylistId: order_by
  Track: Track_order_by
  TrackId: order_by
}

"""primary key columns input for table: PlaylistTrack"""
input PlaylistTrack_pk_columns_input {
  PlaylistId: Int!
  TrackId: Int!
}

"""
select columns of table "PlaylistTrack"
"""
enum PlaylistTrack_select_column {
  """column name"""
  PlaylistId

  """column name"""
  TrackId
}

"""
input type for updating data in table "PlaylistTrack"
"""
input PlaylistTrack_set_input {
  PlaylistId: Int
  TrackId: Int
}

"""aggregate stddev on columns"""
type PlaylistTrack_stddev_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by stddev() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_stddev_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""aggregate stddev_pop on columns"""
type PlaylistTrack_stddev_pop_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by stddev_pop() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_stddev_pop_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""aggregate stddev_samp on columns"""
type PlaylistTrack_stddev_samp_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by stddev_samp() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_stddev_samp_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""
Streaming cursor of the table "PlaylistTrack"
"""
input PlaylistTrack_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: PlaylistTrack_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input PlaylistTrack_stream_cursor_value_input {
  PlaylistId: Int
  TrackId: Int
}

"""aggregate sum on columns"""
type PlaylistTrack_sum_fields {
  PlaylistId: Int
  TrackId: Int
}

"""
order by sum() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_sum_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""
update columns of table "PlaylistTrack"
"""
enum PlaylistTrack_update_column {
  """column name"""
  PlaylistId

  """column name"""
  TrackId
}

input PlaylistTrack_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: PlaylistTrack_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: PlaylistTrack_set_input

  """filter the rows which have to be updated"""
  where: PlaylistTrack_bool_exp!
}

"""aggregate var_pop on columns"""
type PlaylistTrack_var_pop_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by var_pop() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_var_pop_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""aggregate var_samp on columns"""
type PlaylistTrack_var_samp_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by var_samp() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_var_samp_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""aggregate variance on columns"""
type PlaylistTrack_variance_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by variance() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_variance_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""
aggregated selection of "Playlist"
"""
type Playlist_aggregate {
  aggregate: Playlist_aggregate_fields
  nodes: [Playlist!]!
}

"""
aggregate fields of "Playlist"
"""
type Playlist_aggregate_fields {
  avg: Playlist_avg_fields
  count(columns: [Playlist_select_column!], distinct: Boolean): Int!
  max: Playlist_max_fields
  min: Playlist_min_fields
  stddev: Playlist_stddev_fields
  stddev_pop: Playlist_stddev_pop_fields
  stddev_samp: Playlist_stddev_samp_fields
  sum: Playlist_sum_fields
  var_pop: Playlist_var_pop_fields
  var_samp: Playlist_var_samp_fields
  variance: Playlist_variance_fields
}

"""aggregate avg on columns"""
type Playlist_avg_fields {
  PlaylistId: Float
}

"""
Boolean expression to filter rows from the table "Playlist". All fields are combined with a logical 'AND'.
"""
input Playlist_bool_exp {
  Name: String_comparison_exp
  PlaylistId: Int_comparison_exp
  PlaylistTracks: PlaylistTrack_bool_exp
  PlaylistTracks_aggregate: PlaylistTrack_aggregate_bool_exp
  _and: [Playlist_bool_exp!]
  _not: Playlist_bool_exp
  _or: [Playlist_bool_exp!]
}

"""
unique or primary key constraints on table "Playlist"
"""
enum Playlist_constraint {
  """
  unique or primary key constraint on columns "PlaylistId"
  """
  PK_Playlist
}

"""
input type for incrementing numeric columns in table "Playlist"
"""
input Playlist_inc_input {
  PlaylistId: Int
}

"""
input type for inserting data into table "Playlist"
"""
input Playlist_insert_input {
  Name: String
  PlaylistId: Int
  PlaylistTracks: PlaylistTrack_arr_rel_insert_input
}

"""aggregate max on columns"""
type Playlist_max_fields {
  Name: String
  PlaylistId: Int
}

"""aggregate min on columns"""
type Playlist_min_fields {
  Name: String
  PlaylistId: Int
}

"""
response of any mutation on the table "Playlist"
"""
type Playlist_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Playlist!]!
}

"""
input type for inserting object relation for remote table "Playlist"
"""
input Playlist_obj_rel_insert_input {
  data: Playlist_insert_input!

  """upsert condition"""
  on_conflict: Playlist_on_conflict
}

"""
on_conflict condition type for table "Playlist"
"""
input Playlist_on_conflict {
  constraint: Playlist_constraint!
  update_columns: [Playlist_update_column!]! = []
  where: Playlist_bool_exp
}

"""Ordering options when selecting data from "Playlist"."""
input Playlist_order_by {
  Name: order_by
  PlaylistId: order_by
  PlaylistTracks_aggregate: PlaylistTrack_aggregate_order_by
}

"""primary key columns input for table: Playlist"""
input Playlist_pk_columns_input {
  PlaylistId: Int!
}

"""
select columns of table "Playlist"
"""
enum Playlist_select_column {
  """column name"""
  Name

  """column name"""
  PlaylistId
}

"""
input type for updating data in table "Playlist"
"""
input Playlist_set_input {
  Name: String
  PlaylistId: Int
}

"""aggregate stddev on columns"""
type Playlist_stddev_fields {
  PlaylistId: Float
}

"""aggregate stddev_pop on columns"""
type Playlist_stddev_pop_fields {
  PlaylistId: Float
}

"""aggregate stddev_samp on columns"""
type Playlist_stddev_samp_fields {
  PlaylistId: Float
}

"""
Streaming cursor of the table "Playlist"
"""
input Playlist_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Playlist_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Playlist_stream_cursor_value_input {
  Name: String
  PlaylistId: Int
}

"""aggregate sum on columns"""
type Playlist_sum_fields {
  PlaylistId: Int
}

"""
update columns of table "Playlist"
"""
enum Playlist_update_column {
  """column name"""
  Name

  """column name"""
  PlaylistId
}

input Playlist_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Playlist_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Playlist_set_input

  """filter the rows which have to be updated"""
  where: Playlist_bool_exp!
}

"""aggregate var_pop on columns"""
type Playlist_var_pop_fields {
  PlaylistId: Float
}

"""aggregate var_samp on columns"""
type Playlist_var_samp_fields {
  PlaylistId: Float
}

"""aggregate variance on columns"""
type Playlist_variance_fields {
  PlaylistId: Float
}

"""
Boolean expression to compare columns of type "String". All fields are combined with logical 'AND'.
"""
input String_comparison_exp {
  _eq: String
  _gt: String
  _gte: String

  """does the column match the given case-insensitive pattern"""
  _ilike: String
  _in: [String!]

  """
  does the column match the given POSIX regular expression, case insensitive
  """
  _iregex: String
  _is_null: Boolean

  """does the column match the given pattern"""
  _like: String
  _lt: String
  _lte: String
  _neq: String

  """does the column NOT match the given case-insensitive pattern"""
  _nilike: String
  _nin: [String!]

  """
  does the column NOT match the given POSIX regular expression, case insensitive
  """
  _niregex: String

  """does the column NOT match the given pattern"""
  _nlike: String

  """
  does the column NOT match the given POSIX regular expression, case sensitive
  """
  _nregex: String

  """does the column NOT match the given SQL regular expression"""
  _nsimilar: String

  """
  does the column match the given POSIX regular expression, case sensitive
  """
  _regex: String

  """does the column match the given SQL regular expression"""
  _similar: String
}

"""
columns and relationships of "Track"
"""
type Track {
  """An object relationship"""
  Album: Album
  AlbumId: Int
  Bytes: Int
  Composer: String

  """An object relationship"""
  Genre: Genre
  GenreId: Int

  """An array relationship"""
  InvoiceLines(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): [InvoiceLine!]!

  """An aggregate relationship"""
  InvoiceLines_aggregate(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): InvoiceLine_aggregate!

  """An object relationship"""
  MediaType: MediaType!
  MediaTypeId: Int!
  Milliseconds: Int!
  Name: String!

  """An array relationship"""
  PlaylistTracks(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): [PlaylistTrack!]!

  """An aggregate relationship"""
  PlaylistTracks_aggregate(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): PlaylistTrack_aggregate!
  TrackId: Int!
  UnitPrice: numeric!
}

"""
aggregated selection of "Track"
"""
type Track_aggregate {
  aggregate: Track_aggregate_fields
  nodes: [Track!]!
}

input Track_aggregate_bool_exp {
  count: Track_aggregate_bool_exp_count
}

input Track_aggregate_bool_exp_count {
  arguments: [Track_select_column!]
  distinct: Boolean
  filter: Track_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "Track"
"""
type Track_aggregate_fields {
  avg: Track_avg_fields
  count(columns: [Track_select_column!], distinct: Boolean): Int!
  max: Track_max_fields
  min: Track_min_fields
  stddev: Track_stddev_fields
  stddev_pop: Track_stddev_pop_fields
  stddev_samp: Track_stddev_samp_fields
  sum: Track_sum_fields
  var_pop: Track_var_pop_fields
  var_samp: Track_var_samp_fields
  variance: Track_variance_fields
}

"""
order by aggregate values of table "Track"
"""
input Track_aggregate_order_by {
  avg: Track_avg_order_by
  count: order_by
  max: Track_max_order_by
  min: Track_min_order_by
  stddev: Track_stddev_order_by
  stddev_pop: Track_stddev_pop_order_by
  stddev_samp: Track_stddev_samp_order_by
  sum: Track_sum_order_by
  var_pop: Track_var_pop_order_by
  var_samp: Track_var_samp_order_by
  variance: Track_variance_order_by
}

"""
input type for inserting array relation for remote table "Track"
"""
input Track_arr_rel_insert_input {
  data: [Track_insert_input!]!

  """upsert condition"""
  on_conflict: Track_on_conflict
}

"""aggregate avg on columns"""
type Track_avg_fields {
  AlbumId: Float
  Bytes: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by avg() on columns of table "Track"
"""
input Track_avg_order_by {
  AlbumId: order_by
  Bytes: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
Boolean expression to filter rows from the table "Track". All fields are combined with a logical 'AND'.
"""
input Track_bool_exp {
  Album: Album_bool_exp
  AlbumId: Int_comparison_exp
  Bytes: Int_comparison_exp
  Composer: String_comparison_exp
  Genre: Genre_bool_exp
  GenreId: Int_comparison_exp
  InvoiceLines: InvoiceLine_bool_exp
  InvoiceLines_aggregate: InvoiceLine_aggregate_bool_exp
  MediaType: MediaType_bool_exp
  MediaTypeId: Int_comparison_exp
  Milliseconds: Int_comparison_exp
  Name: String_comparison_exp
  PlaylistTracks: PlaylistTrack_bool_exp
  PlaylistTracks_aggregate: PlaylistTrack_aggregate_bool_exp
  TrackId: Int_comparison_exp
  UnitPrice: numeric_comparison_exp
  _and: [Track_bool_exp!]
  _not: Track_bool_exp
  _or: [Track_bool_exp!]
}

"""
unique or primary key constraints on table "Track"
"""
enum Track_constraint {
  """
  unique or primary key constraint on columns "TrackId"
  """
  PK_Track
}

"""
input type for incrementing numeric columns in table "Track"
"""
input Track_inc_input {
  AlbumId: Int
  Bytes: Int
  GenreId: Int
  MediaTypeId: Int
  Milliseconds: Int
  TrackId: Int
  UnitPrice: numeric
}

"""
input type for inserting data into table "Track"
"""
input Track_insert_input {
  Album: Album_obj_rel_insert_input
  AlbumId: Int
  Bytes: Int
  Composer: String
  Genre: Genre_obj_rel_insert_input
  GenreId: Int
  InvoiceLines: InvoiceLine_arr_rel_insert_input
  MediaType: MediaType_obj_rel_insert_input
  MediaTypeId: Int
  Milliseconds: Int
  Name: String
  PlaylistTracks: PlaylistTrack_arr_rel_insert_input
  TrackId: Int
  UnitPrice: numeric
}

"""aggregate max on columns"""
type Track_max_fields {
  AlbumId: Int
  Bytes: Int
  Composer: String
  GenreId: Int
  MediaTypeId: Int
  Milliseconds: Int
  Name: String
  TrackId: Int
  UnitPrice: numeric
}

"""
order by max() on columns of table "Track"
"""
input Track_max_order_by {
  AlbumId: order_by
  Bytes: order_by
  Composer: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  Name: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate min on columns"""
type Track_min_fields {
  AlbumId: Int
  Bytes: Int
  Composer: String
  GenreId: Int
  MediaTypeId: Int
  Milliseconds: Int
  Name: String
  TrackId: Int
  UnitPrice: numeric
}

"""
order by min() on columns of table "Track"
"""
input Track_min_order_by {
  AlbumId: order_by
  Bytes: order_by
  Composer: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  Name: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
response of any mutation on the table "Track"
"""
type Track_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Track!]!
}

"""
input type for inserting object relation for remote table "Track"
"""
input Track_obj_rel_insert_input {
  data: Track_insert_input!

  """upsert condition"""
  on_conflict: Track_on_conflict
}

"""
on_conflict condition type for table "Track"
"""
input Track_on_conflict {
  constraint: Track_constraint!
  update_columns: [Track_update_column!]! = []
  where: Track_bool_exp
}

"""Ordering options when selecting data from "Track"."""
input Track_order_by {
  Album: Album_order_by
  AlbumId: order_by
  Bytes: order_by
  Composer: order_by
  Genre: Genre_order_by
  GenreId: order_by
  InvoiceLines_aggregate: InvoiceLine_aggregate_order_by
  MediaType: MediaType_order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  Name: order_by
  PlaylistTracks_aggregate: PlaylistTrack_aggregate_order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""primary key columns input for table: Track"""
input Track_pk_columns_input {
  TrackId: Int!
}

"""
select columns of table "Track"
"""
enum Track_select_column {
  """column name"""
  AlbumId

  """column name"""
  Bytes

  """column name"""
  Composer

  """column name"""
  GenreId

  """column name"""
  MediaTypeId

  """column name"""
  Milliseconds

  """column name"""
  Name

  """column name"""
  TrackId

  """column name"""
  UnitPrice
}

"""
input type for updating data in table "Track"
"""
input Track_set_input {
  AlbumId: Int
  Bytes: Int
  Composer: String
  GenreId: Int
  MediaTypeId: Int
  Milliseconds: Int
  Name: String
  TrackId: Int
  UnitPrice: numeric
}

"""aggregate stddev on columns"""
type Track_stddev_fields {
  AlbumId: Float
  Bytes: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by stddev() on columns of table "Track"
"""
input Track_stddev_order_by {
  AlbumId: order_by
  Bytes: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate stddev_pop on columns"""
type Track_stddev_pop_fields {
  AlbumId: Float
  Bytes: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by stddev_pop() on columns of table "Track"
"""
input Track_stddev_pop_order_by {
  AlbumId: order_by
  Bytes: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate stddev_samp on columns"""
type Track_stddev_samp_fields {
  AlbumId: Float
  Bytes: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by stddev_samp() on columns of table "Track"
"""
input Track_stddev_samp_order_by {
  AlbumId: order_by
  Bytes: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
Streaming cursor of the table "Track"
"""
input Track_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Track_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Track_stream_cursor_value_input {
  AlbumId: Int
  Bytes: Int
  Composer: String
  GenreId: Int
  MediaTypeId: Int
  Milliseconds: Int
  Name: String
  TrackId: Int
  UnitPrice: numeric
}

"""aggregate sum on columns"""
type Track_sum_fields {
  AlbumId: Int
  Bytes: Int
  GenreId: Int
  MediaTypeId: Int
  Milliseconds: Int
  TrackId: Int
  UnitPrice: numeric
}

"""
order by sum() on columns of table "Track"
"""
input Track_sum_order_by {
  AlbumId: order_by
  Bytes: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
update columns of table "Track"
"""
enum Track_update_column {
  """column name"""
  AlbumId

  """column name"""
  Bytes

  """column name"""
  Composer

  """column name"""
  GenreId

  """column name"""
  MediaTypeId

  """column name"""
  Milliseconds

  """column name"""
  Name

  """column name"""
  TrackId

  """column name"""
  UnitPrice
}

input Track_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Track_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Track_set_input

  """filter the rows which have to be updated"""
  where: Track_bool_exp!
}

"""aggregate var_pop on columns"""
type Track_var_pop_fields {
  AlbumId: Float
  Bytes: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by var_pop() on columns of table "Track"
"""
input Track_var_pop_order_by {
  AlbumId: order_by
  Bytes: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate var_samp on columns"""
type Track_var_samp_fields {
  AlbumId: Float
  Bytes: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by var_samp() on columns of table "Track"
"""
input Track_var_samp_order_by {
  AlbumId: order_by
  Bytes: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate variance on columns"""
type Track_variance_fields {
  AlbumId: Float
  Bytes: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by variance() on columns of table "Track"
"""
input Track_variance_order_by {
  AlbumId: order_by
  Bytes: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""ordering argument of a cursor"""
enum cursor_ordering {
  """ascending ordering of the cursor"""
  ASC

  """descending ordering of the cursor"""
  DESC
}

"""mutation root"""
type mutation_root {
  """
  delete data from the table: "Album"
  """
  delete_Album(
    """filter the rows which have to be deleted"""
    where: Album_bool_exp!
  ): Album_mutation_response

  """
  delete single row from the table: "Album"
  """
  delete_Album_by_pk(AlbumId: Int!): Album

  """
  delete data from the table: "Artist"
  """
  delete_Artist(
    """filter the rows which have to be deleted"""
    where: Artist_bool_exp!
  ): Artist_mutation_response

  """
  delete single row from the table: "Artist"
  """
  delete_Artist_by_pk(ArtistId: Int!): Artist

  """
  delete data from the table: "Customer"
  """
  delete_Customer(
    """filter the rows which have to be deleted"""
    where: Customer_bool_exp!
  ): Customer_mutation_response

  """
  delete single row from the table: "Customer"
  """
  delete_Customer_by_pk(CustomerId: Int!): Customer

  """
  delete data from the table: "Employee"
  """
  delete_Employee(
    """filter the rows which have to be deleted"""
    where: Employee_bool_exp!
  ): Employee_mutation_response

  """
  delete single row from the table: "Employee"
  """
  delete_Employee_by_pk(EmployeeId: Int!): Employee

  """
  delete data from the table: "Genre"
  """
  delete_Genre(
    """filter the rows which have to be deleted"""
    where: Genre_bool_exp!
  ): Genre_mutation_response

  """
  delete single row from the table: "Genre"
  """
  delete_Genre_by_pk(GenreId: Int!): Genre

  """
  delete data from the table: "Invoice"
  """
  delete_Invoice(
    """filter the rows which have to be deleted"""
    where: Invoice_bool_exp!
  ): Invoice_mutation_response

  """
  delete data from the table: "InvoiceLine"
  """
  delete_InvoiceLine(
    """filter the rows which have to be deleted"""
    where: InvoiceLine_bool_exp!
  ): InvoiceLine_mutation_response

  """
  delete single row from the table: "InvoiceLine"
  """
  delete_InvoiceLine_by_pk(InvoiceLineId: Int!): InvoiceLine

  """
  delete single row from the table: "Invoice"
  """
  delete_Invoice_by_pk(InvoiceId: Int!): Invoice

  """
  delete data from the table: "MediaType"
  """
  delete_MediaType(
    """filter the rows which have to be deleted"""
    where: MediaType_bool_exp!
  ): MediaType_mutation_response

  """
  delete single row from the table: "MediaType"
  """
  delete_MediaType_by_pk(MediaTypeId: Int!): MediaType

  """
  delete data from the table: "Playlist"
  """
  delete_Playlist(
    """filter the rows which have to be deleted"""
    where: Playlist_bool_exp!
  ): Playlist_mutation_response

  """
  delete data from the table: "PlaylistTrack"
  """
  delete_PlaylistTrack(
    """filter the rows which have to be deleted"""
    where: PlaylistTrack_bool_exp!
  ): PlaylistTrack_mutation_response

  """
  delete single row from the table: "PlaylistTrack"
  """
  delete_PlaylistTrack_by_pk(PlaylistId: Int!, TrackId: Int!): PlaylistTrack

  """
  delete single row from the table: "Playlist"
  """
  delete_Playlist_by_pk(PlaylistId: Int!): Playlist

  """
  delete data from the table: "Track"
  """
  delete_Track(
    """filter the rows which have to be deleted"""
    where: Track_bool_exp!
  ): Track_mutation_response

  """
  delete single row from the table: "Track"
  """
  delete_Track_by_pk(TrackId: Int!): Track

  """
  insert data into the table: "Album"
  """
  insert_Album(
    """the rows to be inserted"""
    objects: [Album_insert_input!]!

    """upsert condition"""
    on_conflict: Album_on_conflict
  ): Album_mutation_response

  """
  insert a single row into the table: "Album"
  """
  insert_Album_one(
    """the row to be inserted"""
    object: Album_insert_input!

    """upsert condition"""
    on_conflict: Album_on_conflict
  ): Album

  """
  insert data into the table: "Artist"
  """
  insert_Artist(
    """the rows to be inserted"""
    objects: [Artist_insert_input!]!

    """upsert condition"""
    on_conflict: Artist_on_conflict
  ): Artist_mutation_response

  """
  insert a single row into the table: "Artist"
  """
  insert_Artist_one(
    """the row to be inserted"""
    object: Artist_insert_input!

    """upsert condition"""
    on_conflict: Artist_on_conflict
  ): Artist

  """
  insert data into the table: "Customer"
  """
  insert_Customer(
    """the rows to be inserted"""
    objects: [Customer_insert_input!]!

    """upsert condition"""
    on_conflict: Customer_on_conflict
  ): Customer_mutation_response

  """
  insert a single row into the table: "Customer"
  """
  insert_Customer_one(
    """the row to be inserted"""
    object: Customer_insert_input!

    """upsert condition"""
    on_conflict: Customer_on_conflict
  ): Customer

  """
  insert data into the table: "Employee"
  """
  insert_Employee(
    """the rows to be inserted"""
    objects: [Employee_insert_input!]!

    """upsert condition"""
    on_conflict: Employee_on_conflict
  ): Employee_mutation_response

  """
  insert a single row into the table: "Employee"
  """
  insert_Employee_one(
    """the row to be inserted"""
    object: Employee_insert_input!

    """upsert condition"""
    on_conflict: Employee_on_conflict
  ): Employee

  """
  insert data into the table: "Genre"
  """
  insert_Genre(
    """the rows to be inserted"""
    objects: [Genre_insert_input!]!

    """upsert condition"""
    on_conflict: Genre_on_conflict
  ): Genre_mutation_response

  """
  insert a single row into the table: "Genre"
  """
  insert_Genre_one(
    """the row to be inserted"""
    object: Genre_insert_input!

    """upsert condition"""
    on_conflict: Genre_on_conflict
  ): Genre

  """
  insert data into the table: "Invoice"
  """
  insert_Invoice(
    """the rows to be inserted"""
    objects: [Invoice_insert_input!]!

    """upsert condition"""
    on_conflict: Invoice_on_conflict
  ): Invoice_mutation_response

  """
  insert data into the table: "InvoiceLine"
  """
  insert_InvoiceLine(
    """the rows to be inserted"""
    objects: [InvoiceLine_insert_input!]!

    """upsert condition"""
    on_conflict: InvoiceLine_on_conflict
  ): InvoiceLine_mutation_response

  """
  insert a single row into the table: "InvoiceLine"
  """
  insert_InvoiceLine_one(
    """the row to be inserted"""
    object: InvoiceLine_insert_input!

    """upsert condition"""
    on_conflict: InvoiceLine_on_conflict
  ): InvoiceLine

  """
  insert a single row into the table: "Invoice"
  """
  insert_Invoice_one(
    """the row to be inserted"""
    object: Invoice_insert_input!

    """upsert condition"""
    on_conflict: Invoice_on_conflict
  ): Invoice

  """
  insert data into the table: "MediaType"
  """
  insert_MediaType(
    """the rows to be inserted"""
    objects: [MediaType_insert_input!]!

    """upsert condition"""
    on_conflict: MediaType_on_conflict
  ): MediaType_mutation_response

  """
  insert a single row into the table: "MediaType"
  """
  insert_MediaType_one(
    """the row to be inserted"""
    object: MediaType_insert_input!

    """upsert condition"""
    on_conflict: MediaType_on_conflict
  ): MediaType

  """
  insert data into the table: "Playlist"
  """
  insert_Playlist(
    """the rows to be inserted"""
    objects: [Playlist_insert_input!]!

    """upsert condition"""
    on_conflict: Playlist_on_conflict
  ): Playlist_mutation_response

  """
  insert data into the table: "PlaylistTrack"
  """
  insert_PlaylistTrack(
    """the rows to be inserted"""
    objects: [PlaylistTrack_insert_input!]!

    """upsert condition"""
    on_conflict: PlaylistTrack_on_conflict
  ): PlaylistTrack_mutation_response

  """
  insert a single row into the table: "PlaylistTrack"
  """
  insert_PlaylistTrack_one(
    """the row to be inserted"""
    object: PlaylistTrack_insert_input!

    """upsert condition"""
    on_conflict: PlaylistTrack_on_conflict
  ): PlaylistTrack

  """
  insert a single row into the table: "Playlist"
  """
  insert_Playlist_one(
    """the row to be inserted"""
    object: Playlist_insert_input!

    """upsert condition"""
    on_conflict: Playlist_on_conflict
  ): Playlist

  """
  insert data into the table: "Track"
  """
  insert_Track(
    """the rows to be inserted"""
    objects: [Track_insert_input!]!

    """upsert condition"""
    on_conflict: Track_on_conflict
  ): Track_mutation_response

  """
  insert a single row into the table: "Track"
  """
  insert_Track_one(
    """the row to be inserted"""
    object: Track_insert_input!

    """upsert condition"""
    on_conflict: Track_on_conflict
  ): Track

  """
  update data of the table: "Album"
  """
  update_Album(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Album_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Album_set_input

    """filter the rows which have to be updated"""
    where: Album_bool_exp!
  ): Album_mutation_response

  """
  update single row of the table: "Album"
  """
  update_Album_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Album_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Album_set_input
    pk_columns: Album_pk_columns_input!
  ): Album

  """
  update multiples rows of table: "Album"
  """
  update_Album_many(
    """updates to execute, in order"""
    updates: [Album_updates!]!
  ): [Album_mutation_response]

  """
  update data of the table: "Artist"
  """
  update_Artist(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Artist_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Artist_set_input

    """filter the rows which have to be updated"""
    where: Artist_bool_exp!
  ): Artist_mutation_response

  """
  update single row of the table: "Artist"
  """
  update_Artist_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Artist_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Artist_set_input
    pk_columns: Artist_pk_columns_input!
  ): Artist

  """
  update multiples rows of table: "Artist"
  """
  update_Artist_many(
    """updates to execute, in order"""
    updates: [Artist_updates!]!
  ): [Artist_mutation_response]

  """
  update data of the table: "Customer"
  """
  update_Customer(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Customer_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Customer_set_input

    """filter the rows which have to be updated"""
    where: Customer_bool_exp!
  ): Customer_mutation_response

  """
  update single row of the table: "Customer"
  """
  update_Customer_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Customer_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Customer_set_input
    pk_columns: Customer_pk_columns_input!
  ): Customer

  """
  update multiples rows of table: "Customer"
  """
  update_Customer_many(
    """updates to execute, in order"""
    updates: [Customer_updates!]!
  ): [Customer_mutation_response]

  """
  update data of the table: "Employee"
  """
  update_Employee(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Employee_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Employee_set_input

    """filter the rows which have to be updated"""
    where: Employee_bool_exp!
  ): Employee_mutation_response

  """
  update single row of the table: "Employee"
  """
  update_Employee_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Employee_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Employee_set_input
    pk_columns: Employee_pk_columns_input!
  ): Employee

  """
  update multiples rows of table: "Employee"
  """
  update_Employee_many(
    """updates to execute, in order"""
    updates: [Employee_updates!]!
  ): [Employee_mutation_response]

  """
  update data of the table: "Genre"
  """
  update_Genre(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Genre_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Genre_set_input

    """filter the rows which have to be updated"""
    where: Genre_bool_exp!
  ): Genre_mutation_response

  """
  update single row of the table: "Genre"
  """
  update_Genre_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Genre_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Genre_set_input
    pk_columns: Genre_pk_columns_input!
  ): Genre

  """
  update multiples rows of table: "Genre"
  """
  update_Genre_many(
    """updates to execute, in order"""
    updates: [Genre_updates!]!
  ): [Genre_mutation_response]

  """
  update data of the table: "Invoice"
  """
  update_Invoice(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Invoice_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Invoice_set_input

    """filter the rows which have to be updated"""
    where: Invoice_bool_exp!
  ): Invoice_mutation_response

  """
  update data of the table: "InvoiceLine"
  """
  update_InvoiceLine(
    """increments the numeric columns with given value of the filtered values"""
    _inc: InvoiceLine_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: InvoiceLine_set_input

    """filter the rows which have to be updated"""
    where: InvoiceLine_bool_exp!
  ): InvoiceLine_mutation_response

  """
  update single row of the table: "InvoiceLine"
  """
  update_InvoiceLine_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: InvoiceLine_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: InvoiceLine_set_input
    pk_columns: InvoiceLine_pk_columns_input!
  ): InvoiceLine

  """
  update multiples rows of table: "InvoiceLine"
  """
  update_InvoiceLine_many(
    """updates to execute, in order"""
    updates: [InvoiceLine_updates!]!
  ): [InvoiceLine_mutation_response]

  """
  update single row of the table: "Invoice"
  """
  update_Invoice_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Invoice_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Invoice_set_input
    pk_columns: Invoice_pk_columns_input!
  ): Invoice

  """
  update multiples rows of table: "Invoice"
  """
  update_Invoice_many(
    """updates to execute, in order"""
    updates: [Invoice_updates!]!
  ): [Invoice_mutation_response]

  """
  update data of the table: "MediaType"
  """
  update_MediaType(
    """increments the numeric columns with given value of the filtered values"""
    _inc: MediaType_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: MediaType_set_input

    """filter the rows which have to be updated"""
    where: MediaType_bool_exp!
  ): MediaType_mutation_response

  """
  update single row of the table: "MediaType"
  """
  update_MediaType_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: MediaType_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: MediaType_set_input
    pk_columns: MediaType_pk_columns_input!
  ): MediaType

  """
  update multiples rows of table: "MediaType"
  """
  update_MediaType_many(
    """updates to execute, in order"""
    updates: [MediaType_updates!]!
  ): [MediaType_mutation_response]

  """
  update data of the table: "Playlist"
  """
  update_Playlist(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Playlist_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Playlist_set_input

    """filter the rows which have to be updated"""
    where: Playlist_bool_exp!
  ): Playlist_mutation_response

  """
  update data of the table: "PlaylistTrack"
  """
  update_PlaylistTrack(
    """increments the numeric columns with given value of the filtered values"""
    _inc: PlaylistTrack_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: PlaylistTrack_set_input

    """filter the rows which have to be updated"""
    where: PlaylistTrack_bool_exp!
  ): PlaylistTrack_mutation_response

  """
  update single row of the table: "PlaylistTrack"
  """
  update_PlaylistTrack_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: PlaylistTrack_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: PlaylistTrack_set_input
    pk_columns: PlaylistTrack_pk_columns_input!
  ): PlaylistTrack

  """
  update multiples rows of table: "PlaylistTrack"
  """
  update_PlaylistTrack_many(
    """updates to execute, in order"""
    updates: [PlaylistTrack_updates!]!
  ): [PlaylistTrack_mutation_response]

  """
  update single row of the table: "Playlist"
  """
  update_Playlist_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Playlist_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Playlist_set_input
    pk_columns: Playlist_pk_columns_input!
  ): Playlist

  """
  update multiples rows of table: "Playlist"
  """
  update_Playlist_many(
    """updates to execute, in order"""
    updates: [Playlist_updates!]!
  ): [Playlist_mutation_response]

  """
  update data of the table: "Track"
  """
  update_Track(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Track_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Track_set_input

    """filter the rows which have to be updated"""
    where: Track_bool_exp!
  ): Track_mutation_response

  """
  update single row of the table: "Track"
  """
  update_Track_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Track_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Track_set_input
    pk_columns: Track_pk_columns_input!
  ): Track

  """
  update multiples rows of table: "Track"
  """
  update_Track_many(
    """updates to execute, in order"""
    updates: [Track_updates!]!
  ): [Track_mutation_response]
}

scalar numeric

"""
Boolean expression to compare columns of type "numeric". All fields are combined with logical 'AND'.
"""
input numeric_comparison_exp {
  _eq: numeric
  _gt: numeric
  _gte: numeric
  _in: [numeric!]
  _is_null: Boolean
  _lt: numeric
  _lte: numeric
  _neq: numeric
  _nin: [numeric!]
}

"""column ordering options"""
enum order_by {
  """in ascending order, nulls last"""
  asc

  """in ascending order, nulls first"""
  asc_nulls_first

  """in ascending order, nulls last"""
  asc_nulls_last

  """in descending order, nulls first"""
  desc

  """in descending order, nulls first"""
  desc_nulls_first

  """in descending order, nulls last"""
  desc_nulls_last
}

type query_root {
  """
  fetch data from the table: "Album"
  """
  Album(
    """distinct select on columns"""
    distinct_on: [Album_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Album_order_by!]

    """filter the rows returned"""
    where: Album_bool_exp
  ): [Album!]!

  """
  fetch aggregated fields from the table: "Album"
  """
  Album_aggregate(
    """distinct select on columns"""
    distinct_on: [Album_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Album_order_by!]

    """filter the rows returned"""
    where: Album_bool_exp
  ): Album_aggregate!

  """fetch data from the table: "Album" using primary key columns"""
  Album_by_pk(AlbumId: Int!): Album

  """
  fetch data from the table: "Artist"
  """
  Artist(
    """distinct select on columns"""
    distinct_on: [Artist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Artist_order_by!]

    """filter the rows returned"""
    where: Artist_bool_exp
  ): [Artist!]!

  """
  fetch aggregated fields from the table: "Artist"
  """
  Artist_aggregate(
    """distinct select on columns"""
    distinct_on: [Artist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Artist_order_by!]

    """filter the rows returned"""
    where: Artist_bool_exp
  ): Artist_aggregate!

  """fetch data from the table: "Artist" using primary key columns"""
  Artist_by_pk(ArtistId: Int!): Artist

  """
  fetch data from the table: "Customer"
  """
  Customer(
    """distinct select on columns"""
    distinct_on: [Customer_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Customer_order_by!]

    """filter the rows returned"""
    where: Customer_bool_exp
  ): [Customer!]!

  """
  fetch aggregated fields from the table: "Customer"
  """
  Customer_aggregate(
    """distinct select on columns"""
    distinct_on: [Customer_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Customer_order_by!]

    """filter the rows returned"""
    where: Customer_bool_exp
  ): Customer_aggregate!

  """fetch data from the table: "Customer" using primary key columns"""
  Customer_by_pk(CustomerId: Int!): Customer

  """
  fetch data from the table: "Employee"
  """
  Employee(
    """distinct select on columns"""
    distinct_on: [Employee_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Employee_order_by!]

    """filter the rows returned"""
    where: Employee_bool_exp
  ): [Employee!]!

  """
  fetch aggregated fields from the table: "Employee"
  """
  Employee_aggregate(
    """distinct select on columns"""
    distinct_on: [Employee_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Employee_order_by!]

    """filter the rows returned"""
    where: Employee_bool_exp
  ): Employee_aggregate!

  """fetch data from the table: "Employee" using primary key columns"""
  Employee_by_pk(EmployeeId: Int!): Employee

  """
  fetch data from the table: "Genre"
  """
  Genre(
    """distinct select on columns"""
    distinct_on: [Genre_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Genre_order_by!]

    """filter the rows returned"""
    where: Genre_bool_exp
  ): [Genre!]!

  """
  fetch aggregated fields from the table: "Genre"
  """
  Genre_aggregate(
    """distinct select on columns"""
    distinct_on: [Genre_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Genre_order_by!]

    """filter the rows returned"""
    where: Genre_bool_exp
  ): Genre_aggregate!

  """fetch data from the table: "Genre" using primary key columns"""
  Genre_by_pk(GenreId: Int!): Genre

  """
  fetch data from the table: "Invoice"
  """
  Invoice(
    """distinct select on columns"""
    distinct_on: [Invoice_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Invoice_order_by!]

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): [Invoice!]!

  """
  fetch data from the table: "InvoiceLine"
  """
  InvoiceLine(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): [InvoiceLine!]!

  """
  fetch aggregated fields from the table: "InvoiceLine"
  """
  InvoiceLine_aggregate(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): InvoiceLine_aggregate!

  """fetch data from the table: "InvoiceLine" using primary key columns"""
  InvoiceLine_by_pk(InvoiceLineId: Int!): InvoiceLine

  """
  fetch aggregated fields from the table: "Invoice"
  """
  Invoice_aggregate(
    """distinct select on columns"""
    distinct_on: [Invoice_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Invoice_order_by!]

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): Invoice_aggregate!

  """fetch data from the table: "Invoice" using primary key columns"""
  Invoice_by_pk(InvoiceId: Int!): Invoice

  """
  fetch data from the table: "MediaType"
  """
  MediaType(
    """distinct select on columns"""
    distinct_on: [MediaType_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [MediaType_order_by!]

    """filter the rows returned"""
    where: MediaType_bool_exp
  ): [MediaType!]!

  """
  fetch aggregated fields from the table: "MediaType"
  """
  MediaType_aggregate(
    """distinct select on columns"""
    distinct_on: [MediaType_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [MediaType_order_by!]

    """filter the rows returned"""
    where: MediaType_bool_exp
  ): MediaType_aggregate!

  """fetch data from the table: "MediaType" using primary key columns"""
  MediaType_by_pk(MediaTypeId: Int!): MediaType

  """
  fetch data from the table: "Playlist"
  """
  Playlist(
    """distinct select on columns"""
    distinct_on: [Playlist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Playlist_order_by!]

    """filter the rows returned"""
    where: Playlist_bool_exp
  ): [Playlist!]!

  """
  fetch data from the table: "PlaylistTrack"
  """
  PlaylistTrack(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): [PlaylistTrack!]!

  """
  fetch aggregated fields from the table: "PlaylistTrack"
  """
  PlaylistTrack_aggregate(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): PlaylistTrack_aggregate!

  """fetch data from the table: "PlaylistTrack" using primary key columns"""
  PlaylistTrack_by_pk(PlaylistId: Int!, TrackId: Int!): PlaylistTrack

  """
  fetch aggregated fields from the table: "Playlist"
  """
  Playlist_aggregate(
    """distinct select on columns"""
    distinct_on: [Playlist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Playlist_order_by!]

    """filter the rows returned"""
    where: Playlist_bool_exp
  ): Playlist_aggregate!

  """fetch data from the table: "Playlist" using primary key columns"""
  Playlist_by_pk(PlaylistId: Int!): Playlist

  """
  fetch data from the table: "Track"
  """
  Track(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): [Track!]!

  """
  fetch aggregated fields from the table: "Track"
  """
  Track_aggregate(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): Track_aggregate!

  """fetch data from the table: "Track" using primary key columns"""
  Track_by_pk(TrackId: Int!): Track
}

type subscription_root {
  """
  fetch data from the table: "Album"
  """
  Album(
    """distinct select on columns"""
    distinct_on: [Album_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Album_order_by!]

    """filter the rows returned"""
    where: Album_bool_exp
  ): [Album!]!

  """
  fetch aggregated fields from the table: "Album"
  """
  Album_aggregate(
    """distinct select on columns"""
    distinct_on: [Album_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Album_order_by!]

    """filter the rows returned"""
    where: Album_bool_exp
  ): Album_aggregate!

  """fetch data from the table: "Album" using primary key columns"""
  Album_by_pk(AlbumId: Int!): Album

  """
  fetch data from the table in a streaming manner: "Album"
  """
  Album_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Album_stream_cursor_input]!

    """filter the rows returned"""
    where: Album_bool_exp
  ): [Album!]!

  """
  fetch data from the table: "Artist"
  """
  Artist(
    """distinct select on columns"""
    distinct_on: [Artist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Artist_order_by!]

    """filter the rows returned"""
    where: Artist_bool_exp
  ): [Artist!]!

  """
  fetch aggregated fields from the table: "Artist"
  """
  Artist_aggregate(
    """distinct select on columns"""
    distinct_on: [Artist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Artist_order_by!]

    """filter the rows returned"""
    where: Artist_bool_exp
  ): Artist_aggregate!

  """fetch data from the table: "Artist" using primary key columns"""
  Artist_by_pk(ArtistId: Int!): Artist

  """
  fetch data from the table in a streaming manner: "Artist"
  """
  Artist_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Artist_stream_cursor_input]!

    """filter the rows returned"""
    where: Artist_bool_exp
  ): [Artist!]!

  """
  fetch data from the table: "Customer"
  """
  Customer(
    """distinct select on columns"""
    distinct_on: [Customer_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Customer_order_by!]

    """filter the rows returned"""
    where: Customer_bool_exp
  ): [Customer!]!

  """
  fetch aggregated fields from the table: "Customer"
  """
  Customer_aggregate(
    """distinct select on columns"""
    distinct_on: [Customer_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Customer_order_by!]

    """filter the rows returned"""
    where: Customer_bool_exp
  ): Customer_aggregate!

  """fetch data from the table: "Customer" using primary key columns"""
  Customer_by_pk(CustomerId: Int!): Customer

  """
  fetch data from the table in a streaming manner: "Customer"
  """
  Customer_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Customer_stream_cursor_input]!

    """filter the rows returned"""
    where: Customer_bool_exp
  ): [Customer!]!

  """
  fetch data from the table: "Employee"
  """
  Employee(
    """distinct select on columns"""
    distinct_on: [Employee_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Employee_order_by!]

    """filter the rows returned"""
    where: Employee_bool_exp
  ): [Employee!]!

  """
  fetch aggregated fields from the table: "Employee"
  """
  Employee_aggregate(
    """distinct select on columns"""
    distinct_on: [Employee_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Employee_order_by!]

    """filter the rows returned"""
    where: Employee_bool_exp
  ): Employee_aggregate!

  """fetch data from the table: "Employee" using primary key columns"""
  Employee_by_pk(EmployeeId: Int!): Employee

  """
  fetch data from the table in a streaming manner: "Employee"
  """
  Employee_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Employee_stream_cursor_input]!

    """filter the rows returned"""
    where: Employee_bool_exp
  ): [Employee!]!

  """
  fetch data from the table: "Genre"
  """
  Genre(
    """distinct select on columns"""
    distinct_on: [Genre_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Genre_order_by!]

    """filter the rows returned"""
    where: Genre_bool_exp
  ): [Genre!]!

  """
  fetch aggregated fields from the table: "Genre"
  """
  Genre_aggregate(
    """distinct select on columns"""
    distinct_on: [Genre_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Genre_order_by!]

    """filter the rows returned"""
    where: Genre_bool_exp
  ): Genre_aggregate!

  """fetch data from the table: "Genre" using primary key columns"""
  Genre_by_pk(GenreId: Int!): Genre

  """
  fetch data from the table in a streaming manner: "Genre"
  """
  Genre_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Genre_stream_cursor_input]!

    """filter the rows returned"""
    where: Genre_bool_exp
  ): [Genre!]!

  """
  fetch data from the table: "Invoice"
  """
  Invoice(
    """distinct select on columns"""
    distinct_on: [Invoice_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Invoice_order_by!]

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): [Invoice!]!

  """
  fetch data from the table: "InvoiceLine"
  """
  InvoiceLine(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): [InvoiceLine!]!

  """
  fetch aggregated fields from the table: "InvoiceLine"
  """
  InvoiceLine_aggregate(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): InvoiceLine_aggregate!

  """fetch data from the table: "InvoiceLine" using primary key columns"""
  InvoiceLine_by_pk(InvoiceLineId: Int!): InvoiceLine

  """
  fetch data from the table in a streaming manner: "InvoiceLine"
  """
  InvoiceLine_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [InvoiceLine_stream_cursor_input]!

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): [InvoiceLine!]!

  """
  fetch aggregated fields from the table: "Invoice"
  """
  Invoice_aggregate(
    """distinct select on columns"""
    distinct_on: [Invoice_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Invoice_order_by!]

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): Invoice_aggregate!

  """fetch data from the table: "Invoice" using primary key columns"""
  Invoice_by_pk(InvoiceId: Int!): Invoice

  """
  fetch data from the table in a streaming manner: "Invoice"
  """
  Invoice_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Invoice_stream_cursor_input]!

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): [Invoice!]!

  """
  fetch data from the table: "MediaType"
  """
  MediaType(
    """distinct select on columns"""
    distinct_on: [MediaType_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [MediaType_order_by!]

    """filter the rows returned"""
    where: MediaType_bool_exp
  ): [MediaType!]!

  """
  fetch aggregated fields from the table: "MediaType"
  """
  MediaType_aggregate(
    """distinct select on columns"""
    distinct_on: [MediaType_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [MediaType_order_by!]

    """filter the rows returned"""
    where: MediaType_bool_exp
  ): MediaType_aggregate!

  """fetch data from the table: "MediaType" using primary key columns"""
  MediaType_by_pk(MediaTypeId: Int!): MediaType

  """
  fetch data from the table in a streaming manner: "MediaType"
  """
  MediaType_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [MediaType_stream_cursor_input]!

    """filter the rows returned"""
    where: MediaType_bool_exp
  ): [MediaType!]!

  """
  fetch data from the table: "Playlist"
  """
  Playlist(
    """distinct select on columns"""
    distinct_on: [Playlist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Playlist_order_by!]

    """filter the rows returned"""
    where: Playlist_bool_exp
  ): [Playlist!]!

  """
  fetch data from the table: "PlaylistTrack"
  """
  PlaylistTrack(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): [PlaylistTrack!]!

  """
  fetch aggregated fields from the table: "PlaylistTrack"
  """
  PlaylistTrack_aggregate(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): PlaylistTrack_aggregate!

  """fetch data from the table: "PlaylistTrack" using primary key columns"""
  PlaylistTrack_by_pk(PlaylistId: Int!, TrackId: Int!): PlaylistTrack

  """
  fetch data from the table in a streaming manner: "PlaylistTrack"
  """
  PlaylistTrack_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [PlaylistTrack_stream_cursor_input]!

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): [PlaylistTrack!]!

  """
  fetch aggregated fields from the table: "Playlist"
  """
  Playlist_aggregate(
    """distinct select on columns"""
    distinct_on: [Playlist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Playlist_order_by!]

    """filter the rows returned"""
    where: Playlist_bool_exp
  ): Playlist_aggregate!

  """fetch data from the table: "Playlist" using primary key columns"""
  Playlist_by_pk(PlaylistId: Int!): Playlist

  """
  fetch data from the table in a streaming manner: "Playlist"
  """
  Playlist_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Playlist_stream_cursor_input]!

    """filter the rows returned"""
    where: Playlist_bool_exp
  ): [Playlist!]!

  """
  fetch data from the table: "Track"
  """
  Track(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): [Track!]!

  """
  fetch aggregated fields from the table: "Track"
  """
  Track_aggregate(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): Track_aggregate!

  """fetch data from the table: "Track" using primary key columns"""
  Track_by_pk(TrackId: Int!): Track

  """
  fetch data from the table in a streaming manner: "Track"
  """
  Track_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Track_stream_cursor_input]!

    """filter the rows returned"""
    where: Track_bool_exp
  ): [Track!]!
}

scalar timestamp

"""
Boolean expression to compare columns of type "timestamp". All fields are combined with logical 'AND'.
"""
input timestamp_comparison_exp {
  _eq: timestamp
  _gt: timestamp
  _gte: timestamp
  _in: [timestamp!]
  _is_null: Boolean
  _lt: timestamp
  _lte: timestamp
  _neq: timestamp
  _nin: [timestamp!]
}
`;

export const chinook2 = `
schema {
  query: query_root
  mutation: mutation_root
  subscription: subscription_root
}

"""whether this query should be cached (Hasura Cloud only)"""
directive @cached(
  """measured in seconds"""
  ttl: Int! = 60

  """refresh the cache entry"""
  refresh: Boolean! = false
) on QUERY

"""
columns and relationships of "Album"
"""
type Album {
  AlbumId: Int!

  """An object relationship"""
  Artist: Artist!
  ArtistId: Int!
  Title: String!

  """An array relationship"""
  Tracks(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): [Track!]!

  """An aggregate relationship"""
  Tracks_aggregate(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): Track_aggregate!
}

"""
aggregated selection of "Album"
"""
type Album_aggregate {
  aggregate: Album_aggregate_fields
  nodes: [Album!]!
}

input Album_aggregate_bool_exp {
  count: Album_aggregate_bool_exp_count
}

input Album_aggregate_bool_exp_count {
  arguments: [Album_select_column!]
  distinct: Boolean
  filter: Album_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "Album"
"""
type Album_aggregate_fields {
  avg: Album_avg_fields
  count(columns: [Album_select_column!], distinct: Boolean): Int!
  max: Album_max_fields
  min: Album_min_fields
  stddev: Album_stddev_fields
  stddev_pop: Album_stddev_pop_fields
  stddev_samp: Album_stddev_samp_fields
  sum: Album_sum_fields
  var_pop: Album_var_pop_fields
  var_samp: Album_var_samp_fields
  variance: Album_variance_fields
}

"""
order by aggregate values of table "Album"
"""
input Album_aggregate_order_by {
  avg: Album_avg_order_by
  count: order_by
  max: Album_max_order_by
  min: Album_min_order_by
  stddev: Album_stddev_order_by
  stddev_pop: Album_stddev_pop_order_by
  stddev_samp: Album_stddev_samp_order_by
  sum: Album_sum_order_by
  var_pop: Album_var_pop_order_by
  var_samp: Album_var_samp_order_by
  variance: Album_variance_order_by
}

"""
input type for inserting array relation for remote table "Album"
"""
input Album_arr_rel_insert_input {
  data: [Album_insert_input!]!

  """upsert condition"""
  on_conflict: Album_on_conflict
}

"""aggregate avg on columns"""
type Album_avg_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by avg() on columns of table "Album"
"""
input Album_avg_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""
Boolean expression to filter rows from the table "Album". All fields are combined with a logical 'AND'.
"""
input Album_bool_exp {
  AlbumId: Int_comparison_exp
  Artist: Artist_bool_exp
  ArtistId: Int_comparison_exp
  Title: String_comparison_exp
  Tracks: Track_bool_exp
  Tracks_aggregate: Track_aggregate_bool_exp
  _and: [Album_bool_exp!]
  _not: Album_bool_exp
  _or: [Album_bool_exp!]
}

"""
unique or primary key constraints on table "Album"
"""
enum Album_constraint {
  """
  unique or primary key constraint on columns "AlbumId"
  """
  PK_Album
}

"""
input type for incrementing numeric columns in table "Album"
"""
input Album_inc_input {
  AlbumId: Int
  ArtistId: Int
}

"""
input type for inserting data into table "Album"
"""
input Album_insert_input {
  AlbumId: Int
  Artist: Artist_obj_rel_insert_input
  ArtistId: Int
  Title: String
  Tracks: Track_arr_rel_insert_input
}

"""aggregate max on columns"""
type Album_max_fields {
  AlbumId: Int
  ArtistId: Int
  Title: String
}

"""
order by max() on columns of table "Album"
"""
input Album_max_order_by {
  AlbumId: order_by
  ArtistId: order_by
  Title: order_by
}

"""aggregate min on columns"""
type Album_min_fields {
  AlbumId: Int
  ArtistId: Int
  Title: String
}

"""
order by min() on columns of table "Album"
"""
input Album_min_order_by {
  AlbumId: order_by
  ArtistId: order_by
  Title: order_by
}

"""
response of any mutation on the table "Album"
"""
type Album_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Album!]!
}

"""
input type for inserting object relation for remote table "Album"
"""
input Album_obj_rel_insert_input {
  data: Album_insert_input!

  """upsert condition"""
  on_conflict: Album_on_conflict
}

"""
on_conflict condition type for table "Album"
"""
input Album_on_conflict {
  constraint: Album_constraint!
  update_columns: [Album_update_column!]! = []
  where: Album_bool_exp
}

"""Ordering options when selecting data from "Album"."""
input Album_order_by {
  AlbumId: order_by
  Artist: Artist_order_by
  ArtistId: order_by
  Title: order_by
  Tracks_aggregate: Track_aggregate_order_by
}

"""primary key columns input for table: Album"""
input Album_pk_columns_input {
  AlbumId: Int!
}

"""
select columns of table "Album"
"""
enum Album_select_column {
  """column name"""
  AlbumId

  """column name"""
  ArtistId

  """column name"""
  Title
}

"""
input type for updating data in table "Album"
"""
input Album_set_input {
  AlbumId: Int
  ArtistId: Int
  Title: String
}

"""aggregate stddev on columns"""
type Album_stddev_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by stddev() on columns of table "Album"
"""
input Album_stddev_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""aggregate stddev_pop on columns"""
type Album_stddev_pop_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by stddev_pop() on columns of table "Album"
"""
input Album_stddev_pop_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""aggregate stddev_samp on columns"""
type Album_stddev_samp_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by stddev_samp() on columns of table "Album"
"""
input Album_stddev_samp_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""
Streaming cursor of the table "Album"
"""
input Album_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Album_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Album_stream_cursor_value_input {
  AlbumId: Int
  ArtistId: Int
  Title: String
}

"""aggregate sum on columns"""
type Album_sum_fields {
  AlbumId: Int
  ArtistId: Int
}

"""
order by sum() on columns of table "Album"
"""
input Album_sum_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""
update columns of table "Album"
"""
enum Album_update_column {
  """column name"""
  AlbumId

  """column name"""
  ArtistId

  """column name"""
  Title
}

input Album_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Album_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Album_set_input

  """filter the rows which have to be updated"""
  where: Album_bool_exp!
}

"""aggregate var_pop on columns"""
type Album_var_pop_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by var_pop() on columns of table "Album"
"""
input Album_var_pop_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""aggregate var_samp on columns"""
type Album_var_samp_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by var_samp() on columns of table "Album"
"""
input Album_var_samp_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""aggregate variance on columns"""
type Album_variance_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by variance() on columns of table "Album"
"""
input Album_variance_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""
columns and relationships of "Artist"
"""
type Artist {
  """An array relationship"""
  Albums(
    """distinct select on columns"""
    distinct_on: [Album_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Album_order_by!]

    """filter the rows returned"""
    where: Album_bool_exp
  ): [Album!]!

  """An aggregate relationship"""
  Albums_aggregate(
    """distinct select on columns"""
    distinct_on: [Album_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Album_order_by!]

    """filter the rows returned"""
    where: Album_bool_exp
  ): Album_aggregate!
  ArtistId: Int!
  Name: String
}

"""
aggregated selection of "Artist"
"""
type Artist_aggregate {
  aggregate: Artist_aggregate_fields
  nodes: [Artist!]!
}

"""
aggregate fields of "Artist"
"""
type Artist_aggregate_fields {
  avg: Artist_avg_fields
  count(columns: [Artist_select_column!], distinct: Boolean): Int!
  max: Artist_max_fields
  min: Artist_min_fields
  stddev: Artist_stddev_fields
  stddev_pop: Artist_stddev_pop_fields
  stddev_samp: Artist_stddev_samp_fields
  sum: Artist_sum_fields
  var_pop: Artist_var_pop_fields
  var_samp: Artist_var_samp_fields
  variance: Artist_variance_fields
}

"""aggregate avg on columns"""
type Artist_avg_fields {
  ArtistId: Float
}

"""
Boolean expression to filter rows from the table "Artist". All fields are combined with a logical 'AND'.
"""
input Artist_bool_exp {
  Albums: Album_bool_exp
  Albums_aggregate: Album_aggregate_bool_exp
  ArtistId: Int_comparison_exp
  Name: String_comparison_exp
  _and: [Artist_bool_exp!]
  _not: Artist_bool_exp
  _or: [Artist_bool_exp!]
}

"""
unique or primary key constraints on table "Artist"
"""
enum Artist_constraint {
  """
  unique or primary key constraint on columns "ArtistId"
  """
  PK_Artist
}

"""
input type for incrementing numeric columns in table "Artist"
"""
input Artist_inc_input {
  ArtistId: Int
}

"""
input type for inserting data into table "Artist"
"""
input Artist_insert_input {
  Albums: Album_arr_rel_insert_input
  ArtistId: Int
  Name: String
}

"""aggregate max on columns"""
type Artist_max_fields {
  ArtistId: Int
  Name: String
}

"""aggregate min on columns"""
type Artist_min_fields {
  ArtistId: Int
  Name: String
}

"""
response of any mutation on the table "Artist"
"""
type Artist_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Artist!]!
}

"""
input type for inserting object relation for remote table "Artist"
"""
input Artist_obj_rel_insert_input {
  data: Artist_insert_input!

  """upsert condition"""
  on_conflict: Artist_on_conflict
}

"""
on_conflict condition type for table "Artist"
"""
input Artist_on_conflict {
  constraint: Artist_constraint!
  update_columns: [Artist_update_column!]! = []
  where: Artist_bool_exp
}

"""Ordering options when selecting data from "Artist"."""
input Artist_order_by {
  Albums_aggregate: Album_aggregate_order_by
  ArtistId: order_by
  Name: order_by
}

"""primary key columns input for table: Artist"""
input Artist_pk_columns_input {
  ArtistId: Int!
}

"""
select columns of table "Artist"
"""
enum Artist_select_column {
  """column name"""
  ArtistId

  """column name"""
  Name
}

"""
input type for updating data in table "Artist"
"""
input Artist_set_input {
  ArtistId: Int
  Name: String
}

"""aggregate stddev on columns"""
type Artist_stddev_fields {
  ArtistId: Float
}

"""aggregate stddev_pop on columns"""
type Artist_stddev_pop_fields {
  ArtistId: Float
}

"""aggregate stddev_samp on columns"""
type Artist_stddev_samp_fields {
  ArtistId: Float
}

"""
Streaming cursor of the table "Artist"
"""
input Artist_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Artist_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Artist_stream_cursor_value_input {
  ArtistId: Int
  Name: String
}

"""aggregate sum on columns"""
type Artist_sum_fields {
  ArtistId: Int
}

"""
update columns of table "Artist"
"""
enum Artist_update_column {
  """column name"""
  ArtistId

  """column name"""
  Name
}

input Artist_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Artist_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Artist_set_input

  """filter the rows which have to be updated"""
  where: Artist_bool_exp!
}

"""aggregate var_pop on columns"""
type Artist_var_pop_fields {
  ArtistId: Float
}

"""aggregate var_samp on columns"""
type Artist_var_samp_fields {
  ArtistId: Float
}

"""aggregate variance on columns"""
type Artist_variance_fields {
  ArtistId: Float
}

"""
columns and relationships of "Customer"
"""
type Customer {
  Address: String
  Company: String
  Country: String
  CustomerId: Int!
  Email: String!

  """An object relationship"""
  Employee: Employee
  FirstName: String!

  """An array relationship"""
  Invoices(
    """distinct select on columns"""
    distinct_on: [Invoice_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Invoice_order_by!]

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): [Invoice!]!

  """An aggregate relationship"""
  Invoices_aggregate(
    """distinct select on columns"""
    distinct_on: [Invoice_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Invoice_order_by!]

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): Invoice_aggregate!
  LastName: String!
  Phone: String
  PostalCode: String
  State: String
  SupportRepId: Int
}

"""
aggregated selection of "Customer"
"""
type Customer_aggregate {
  aggregate: Customer_aggregate_fields
  nodes: [Customer!]!
}

input Customer_aggregate_bool_exp {
  count: Customer_aggregate_bool_exp_count
}

input Customer_aggregate_bool_exp_count {
  arguments: [Customer_select_column!]
  distinct: Boolean
  filter: Customer_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "Customer"
"""
type Customer_aggregate_fields {
  avg: Customer_avg_fields
  count(columns: [Customer_select_column!], distinct: Boolean): Int!
  max: Customer_max_fields
  min: Customer_min_fields
  stddev: Customer_stddev_fields
  stddev_pop: Customer_stddev_pop_fields
  stddev_samp: Customer_stddev_samp_fields
  sum: Customer_sum_fields
  var_pop: Customer_var_pop_fields
  var_samp: Customer_var_samp_fields
  variance: Customer_variance_fields
}

"""
order by aggregate values of table "Customer"
"""
input Customer_aggregate_order_by {
  avg: Customer_avg_order_by
  count: order_by
  max: Customer_max_order_by
  min: Customer_min_order_by
  stddev: Customer_stddev_order_by
  stddev_pop: Customer_stddev_pop_order_by
  stddev_samp: Customer_stddev_samp_order_by
  sum: Customer_sum_order_by
  var_pop: Customer_var_pop_order_by
  var_samp: Customer_var_samp_order_by
  variance: Customer_variance_order_by
}

"""
input type for inserting array relation for remote table "Customer"
"""
input Customer_arr_rel_insert_input {
  data: [Customer_insert_input!]!

  """upsert condition"""
  on_conflict: Customer_on_conflict
}

"""aggregate avg on columns"""
type Customer_avg_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by avg() on columns of table "Customer"
"""
input Customer_avg_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""
Boolean expression to filter rows from the table "Customer". All fields are combined with a logical 'AND'.
"""
input Customer_bool_exp {
  Address: String_comparison_exp
  Company: String_comparison_exp
  Country: String_comparison_exp
  CustomerId: Int_comparison_exp
  Email: String_comparison_exp
  Employee: Employee_bool_exp
  FirstName: String_comparison_exp
  Invoices: Invoice_bool_exp
  Invoices_aggregate: Invoice_aggregate_bool_exp
  LastName: String_comparison_exp
  Phone: String_comparison_exp
  PostalCode: String_comparison_exp
  State: String_comparison_exp
  SupportRepId: Int_comparison_exp
  _and: [Customer_bool_exp!]
  _not: Customer_bool_exp
  _or: [Customer_bool_exp!]
}

"""
unique or primary key constraints on table "Customer"
"""
enum Customer_constraint {
  """
  unique or primary key constraint on columns "CustomerId"
  """
  PK_Customer
}

"""
input type for incrementing numeric columns in table "Customer"
"""
input Customer_inc_input {
  CustomerId: Int
  SupportRepId: Int
}

"""
input type for inserting data into table "Customer"
"""
input Customer_insert_input {
  Address: String
  Company: String
  Country: String
  CustomerId: Int
  Email: String
  Employee: Employee_obj_rel_insert_input
  FirstName: String
  Invoices: Invoice_arr_rel_insert_input
  LastName: String
  Phone: String
  PostalCode: String
  State: String
  SupportRepId: Int
}

"""aggregate max on columns"""
type Customer_max_fields {
  Address: String
  Company: String
  Country: String
  CustomerId: Int
  Email: String
  FirstName: String
  LastName: String
  Phone: String
  PostalCode: String
  State: String
  SupportRepId: Int
}

"""
order by max() on columns of table "Customer"
"""
input Customer_max_order_by {
  Address: order_by
  Company: order_by
  Country: order_by
  CustomerId: order_by
  Email: order_by
  FirstName: order_by
  LastName: order_by
  Phone: order_by
  PostalCode: order_by
  State: order_by
  SupportRepId: order_by
}

"""aggregate min on columns"""
type Customer_min_fields {
  Address: String
  Company: String
  Country: String
  CustomerId: Int
  Email: String
  FirstName: String
  LastName: String
  Phone: String
  PostalCode: String
  State: String
  SupportRepId: Int
}

"""
order by min() on columns of table "Customer"
"""
input Customer_min_order_by {
  Address: order_by
  Company: order_by
  Country: order_by
  CustomerId: order_by
  Email: order_by
  FirstName: order_by
  LastName: order_by
  Phone: order_by
  PostalCode: order_by
  State: order_by
  SupportRepId: order_by
}

"""
response of any mutation on the table "Customer"
"""
type Customer_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Customer!]!
}

"""
input type for inserting object relation for remote table "Customer"
"""
input Customer_obj_rel_insert_input {
  data: Customer_insert_input!

  """upsert condition"""
  on_conflict: Customer_on_conflict
}

"""
on_conflict condition type for table "Customer"
"""
input Customer_on_conflict {
  constraint: Customer_constraint!
  update_columns: [Customer_update_column!]! = []
  where: Customer_bool_exp
}

"""Ordering options when selecting data from "Customer"."""
input Customer_order_by {
  Address: order_by
  Company: order_by
  Country: order_by
  CustomerId: order_by
  Email: order_by
  Employee: Employee_order_by
  FirstName: order_by
  Invoices_aggregate: Invoice_aggregate_order_by
  LastName: order_by
  Phone: order_by
  PostalCode: order_by
  State: order_by
  SupportRepId: order_by
}

"""primary key columns input for table: Customer"""
input Customer_pk_columns_input {
  CustomerId: Int!
}

"""
select columns of table "Customer"
"""
enum Customer_select_column {
  """column name"""
  Address

  """column name"""
  Company

  """column name"""
  Country

  """column name"""
  CustomerId

  """column name"""
  Email

  """column name"""
  FirstName

  """column name"""
  LastName

  """column name"""
  Phone

  """column name"""
  PostalCode

  """column name"""
  State

  """column name"""
  SupportRepId
}

"""
input type for updating data in table "Customer"
"""
input Customer_set_input {
  Address: String
  Company: String
  Country: String
  CustomerId: Int
  Email: String
  FirstName: String
  LastName: String
  Phone: String
  PostalCode: String
  State: String
  SupportRepId: Int
}

"""aggregate stddev on columns"""
type Customer_stddev_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by stddev() on columns of table "Customer"
"""
input Customer_stddev_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""aggregate stddev_pop on columns"""
type Customer_stddev_pop_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by stddev_pop() on columns of table "Customer"
"""
input Customer_stddev_pop_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""aggregate stddev_samp on columns"""
type Customer_stddev_samp_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by stddev_samp() on columns of table "Customer"
"""
input Customer_stddev_samp_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""
Streaming cursor of the table "Customer"
"""
input Customer_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Customer_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Customer_stream_cursor_value_input {
  Address: String
  Company: String
  Country: String
  CustomerId: Int
  Email: String
  FirstName: String
  LastName: String
  Phone: String
  PostalCode: String
  State: String
  SupportRepId: Int
}

"""aggregate sum on columns"""
type Customer_sum_fields {
  CustomerId: Int
  SupportRepId: Int
}

"""
order by sum() on columns of table "Customer"
"""
input Customer_sum_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""
update columns of table "Customer"
"""
enum Customer_update_column {
  """column name"""
  Address

  """column name"""
  Company

  """column name"""
  Country

  """column name"""
  CustomerId

  """column name"""
  Email

  """column name"""
  FirstName

  """column name"""
  LastName

  """column name"""
  Phone

  """column name"""
  PostalCode

  """column name"""
  State

  """column name"""
  SupportRepId
}

input Customer_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Customer_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Customer_set_input

  """filter the rows which have to be updated"""
  where: Customer_bool_exp!
}

"""aggregate var_pop on columns"""
type Customer_var_pop_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by var_pop() on columns of table "Customer"
"""
input Customer_var_pop_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""aggregate var_samp on columns"""
type Customer_var_samp_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by var_samp() on columns of table "Customer"
"""
input Customer_var_samp_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""aggregate variance on columns"""
type Customer_variance_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by variance() on columns of table "Customer"
"""
input Customer_variance_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""
columns and relationships of "Employee"
"""
type Employee {
  Address: String
  BirthDate: timestamp
  City: String
  Country: String

  """An array relationship"""
  Customers(
    """distinct select on columns"""
    distinct_on: [Customer_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Customer_order_by!]

    """filter the rows returned"""
    where: Customer_bool_exp
  ): [Customer!]!

  """An aggregate relationship"""
  Customers_aggregate(
    """distinct select on columns"""
    distinct_on: [Customer_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Customer_order_by!]

    """filter the rows returned"""
    where: Customer_bool_exp
  ): Customer_aggregate!
  Email: String

  """An object relationship"""
  Employee: Employee
  EmployeeId: Int!

  """An array relationship"""
  Employees(
    """distinct select on columns"""
    distinct_on: [Employee_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Employee_order_by!]

    """filter the rows returned"""
    where: Employee_bool_exp
  ): [Employee!]!

  """An aggregate relationship"""
  Employees_aggregate(
    """distinct select on columns"""
    distinct_on: [Employee_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Employee_order_by!]

    """filter the rows returned"""
    where: Employee_bool_exp
  ): Employee_aggregate!
  Fax: String
  FirstName: String!
  HireDate: timestamp
  LastName: String!
  Phone: String
  PostalCode: String
  ReportsTo: Int
  State: String
  Title: String
}

"""
aggregated selection of "Employee"
"""
type Employee_aggregate {
  aggregate: Employee_aggregate_fields
  nodes: [Employee!]!
}

input Employee_aggregate_bool_exp {
  count: Employee_aggregate_bool_exp_count
}

input Employee_aggregate_bool_exp_count {
  arguments: [Employee_select_column!]
  distinct: Boolean
  filter: Employee_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "Employee"
"""
type Employee_aggregate_fields {
  avg: Employee_avg_fields
  count(columns: [Employee_select_column!], distinct: Boolean): Int!
  max: Employee_max_fields
  min: Employee_min_fields
  stddev: Employee_stddev_fields
  stddev_pop: Employee_stddev_pop_fields
  stddev_samp: Employee_stddev_samp_fields
  sum: Employee_sum_fields
  var_pop: Employee_var_pop_fields
  var_samp: Employee_var_samp_fields
  variance: Employee_variance_fields
}

"""
order by aggregate values of table "Employee"
"""
input Employee_aggregate_order_by {
  avg: Employee_avg_order_by
  count: order_by
  max: Employee_max_order_by
  min: Employee_min_order_by
  stddev: Employee_stddev_order_by
  stddev_pop: Employee_stddev_pop_order_by
  stddev_samp: Employee_stddev_samp_order_by
  sum: Employee_sum_order_by
  var_pop: Employee_var_pop_order_by
  var_samp: Employee_var_samp_order_by
  variance: Employee_variance_order_by
}

"""
input type for inserting array relation for remote table "Employee"
"""
input Employee_arr_rel_insert_input {
  data: [Employee_insert_input!]!

  """upsert condition"""
  on_conflict: Employee_on_conflict
}

"""aggregate avg on columns"""
type Employee_avg_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by avg() on columns of table "Employee"
"""
input Employee_avg_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""
Boolean expression to filter rows from the table "Employee". All fields are combined with a logical 'AND'.
"""
input Employee_bool_exp {
  Address: String_comparison_exp
  BirthDate: timestamp_comparison_exp
  City: String_comparison_exp
  Country: String_comparison_exp
  Customers: Customer_bool_exp
  Customers_aggregate: Customer_aggregate_bool_exp
  Email: String_comparison_exp
  Employee: Employee_bool_exp
  EmployeeId: Int_comparison_exp
  Employees: Employee_bool_exp
  Employees_aggregate: Employee_aggregate_bool_exp
  Fax: String_comparison_exp
  FirstName: String_comparison_exp
  HireDate: timestamp_comparison_exp
  LastName: String_comparison_exp
  Phone: String_comparison_exp
  PostalCode: String_comparison_exp
  ReportsTo: Int_comparison_exp
  State: String_comparison_exp
  Title: String_comparison_exp
  _and: [Employee_bool_exp!]
  _not: Employee_bool_exp
  _or: [Employee_bool_exp!]
}

"""
unique or primary key constraints on table "Employee"
"""
enum Employee_constraint {
  """
  unique or primary key constraint on columns "EmployeeId"
  """
  PK_Employee
}

"""
input type for incrementing numeric columns in table "Employee"
"""
input Employee_inc_input {
  EmployeeId: Int
  ReportsTo: Int
}

"""
input type for inserting data into table "Employee"
"""
input Employee_insert_input {
  Address: String
  BirthDate: timestamp
  City: String
  Country: String
  Customers: Customer_arr_rel_insert_input
  Email: String
  Employee: Employee_obj_rel_insert_input
  EmployeeId: Int
  Employees: Employee_arr_rel_insert_input
  Fax: String
  FirstName: String
  HireDate: timestamp
  LastName: String
  Phone: String
  PostalCode: String
  ReportsTo: Int
  State: String
  Title: String
}

"""aggregate max on columns"""
type Employee_max_fields {
  Address: String
  BirthDate: timestamp
  City: String
  Country: String
  Email: String
  EmployeeId: Int
  Fax: String
  FirstName: String
  HireDate: timestamp
  LastName: String
  Phone: String
  PostalCode: String
  ReportsTo: Int
  State: String
  Title: String
}

"""
order by max() on columns of table "Employee"
"""
input Employee_max_order_by {
  Address: order_by
  BirthDate: order_by
  City: order_by
  Country: order_by
  Email: order_by
  EmployeeId: order_by
  Fax: order_by
  FirstName: order_by
  HireDate: order_by
  LastName: order_by
  Phone: order_by
  PostalCode: order_by
  ReportsTo: order_by
  State: order_by
  Title: order_by
}

"""aggregate min on columns"""
type Employee_min_fields {
  Address: String
  BirthDate: timestamp
  City: String
  Country: String
  Email: String
  EmployeeId: Int
  Fax: String
  FirstName: String
  HireDate: timestamp
  LastName: String
  Phone: String
  PostalCode: String
  ReportsTo: Int
  State: String
  Title: String
}

"""
order by min() on columns of table "Employee"
"""
input Employee_min_order_by {
  Address: order_by
  BirthDate: order_by
  City: order_by
  Country: order_by
  Email: order_by
  EmployeeId: order_by
  Fax: order_by
  FirstName: order_by
  HireDate: order_by
  LastName: order_by
  Phone: order_by
  PostalCode: order_by
  ReportsTo: order_by
  State: order_by
  Title: order_by
}

"""
response of any mutation on the table "Employee"
"""
type Employee_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Employee!]!
}

"""
input type for inserting object relation for remote table "Employee"
"""
input Employee_obj_rel_insert_input {
  data: Employee_insert_input!

  """upsert condition"""
  on_conflict: Employee_on_conflict
}

"""
on_conflict condition type for table "Employee"
"""
input Employee_on_conflict {
  constraint: Employee_constraint!
  update_columns: [Employee_update_column!]! = []
  where: Employee_bool_exp
}

"""Ordering options when selecting data from "Employee"."""
input Employee_order_by {
  Address: order_by
  BirthDate: order_by
  City: order_by
  Country: order_by
  Customers_aggregate: Customer_aggregate_order_by
  Email: order_by
  Employee: Employee_order_by
  EmployeeId: order_by
  Employees_aggregate: Employee_aggregate_order_by
  Fax: order_by
  FirstName: order_by
  HireDate: order_by
  LastName: order_by
  Phone: order_by
  PostalCode: order_by
  ReportsTo: order_by
  State: order_by
  Title: order_by
}

"""primary key columns input for table: Employee"""
input Employee_pk_columns_input {
  EmployeeId: Int!
}

"""
select columns of table "Employee"
"""
enum Employee_select_column {
  """column name"""
  Address

  """column name"""
  BirthDate

  """column name"""
  City

  """column name"""
  Country

  """column name"""
  Email

  """column name"""
  EmployeeId

  """column name"""
  Fax

  """column name"""
  FirstName

  """column name"""
  HireDate

  """column name"""
  LastName

  """column name"""
  Phone

  """column name"""
  PostalCode

  """column name"""
  ReportsTo

  """column name"""
  State

  """column name"""
  Title
}

"""
input type for updating data in table "Employee"
"""
input Employee_set_input {
  Address: String
  BirthDate: timestamp
  City: String
  Country: String
  Email: String
  EmployeeId: Int
  Fax: String
  FirstName: String
  HireDate: timestamp
  LastName: String
  Phone: String
  PostalCode: String
  ReportsTo: Int
  State: String
  Title: String
}

"""aggregate stddev on columns"""
type Employee_stddev_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by stddev() on columns of table "Employee"
"""
input Employee_stddev_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""aggregate stddev_pop on columns"""
type Employee_stddev_pop_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by stddev_pop() on columns of table "Employee"
"""
input Employee_stddev_pop_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""aggregate stddev_samp on columns"""
type Employee_stddev_samp_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by stddev_samp() on columns of table "Employee"
"""
input Employee_stddev_samp_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""
Streaming cursor of the table "Employee"
"""
input Employee_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Employee_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Employee_stream_cursor_value_input {
  Address: String
  BirthDate: timestamp
  City: String
  Country: String
  Email: String
  EmployeeId: Int
  Fax: String
  FirstName: String
  HireDate: timestamp
  LastName: String
  Phone: String
  PostalCode: String
  ReportsTo: Int
  State: String
  Title: String
}

"""aggregate sum on columns"""
type Employee_sum_fields {
  EmployeeId: Int
  ReportsTo: Int
}

"""
order by sum() on columns of table "Employee"
"""
input Employee_sum_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""
update columns of table "Employee"
"""
enum Employee_update_column {
  """column name"""
  Address

  """column name"""
  BirthDate

  """column name"""
  City

  """column name"""
  Country

  """column name"""
  Email

  """column name"""
  EmployeeId

  """column name"""
  Fax

  """column name"""
  FirstName

  """column name"""
  HireDate

  """column name"""
  LastName

  """column name"""
  Phone

  """column name"""
  PostalCode

  """column name"""
  ReportsTo

  """column name"""
  State

  """column name"""
  Title
}

input Employee_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Employee_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Employee_set_input

  """filter the rows which have to be updated"""
  where: Employee_bool_exp!
}

"""aggregate var_pop on columns"""
type Employee_var_pop_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by var_pop() on columns of table "Employee"
"""
input Employee_var_pop_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""aggregate var_samp on columns"""
type Employee_var_samp_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by var_samp() on columns of table "Employee"
"""
input Employee_var_samp_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""aggregate variance on columns"""
type Employee_variance_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by variance() on columns of table "Employee"
"""
input Employee_variance_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""
columns and relationships of "Genre"
"""
type Genre {
  GenreId: Int!
  Name: String

  """An array relationship"""
  Tracks(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): [Track!]!

  """An aggregate relationship"""
  Tracks_aggregate(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): Track_aggregate!
}

"""
aggregated selection of "Genre"
"""
type Genre_aggregate {
  aggregate: Genre_aggregate_fields
  nodes: [Genre!]!
}

"""
aggregate fields of "Genre"
"""
type Genre_aggregate_fields {
  avg: Genre_avg_fields
  count(columns: [Genre_select_column!], distinct: Boolean): Int!
  max: Genre_max_fields
  min: Genre_min_fields
  stddev: Genre_stddev_fields
  stddev_pop: Genre_stddev_pop_fields
  stddev_samp: Genre_stddev_samp_fields
  sum: Genre_sum_fields
  var_pop: Genre_var_pop_fields
  var_samp: Genre_var_samp_fields
  variance: Genre_variance_fields
}

"""aggregate avg on columns"""
type Genre_avg_fields {
  GenreId: Float
}

"""
Boolean expression to filter rows from the table "Genre". All fields are combined with a logical 'AND'.
"""
input Genre_bool_exp {
  GenreId: Int_comparison_exp
  Name: String_comparison_exp
  Tracks: Track_bool_exp
  Tracks_aggregate: Track_aggregate_bool_exp
  _and: [Genre_bool_exp!]
  _not: Genre_bool_exp
  _or: [Genre_bool_exp!]
}

"""
unique or primary key constraints on table "Genre"
"""
enum Genre_constraint {
  """
  unique or primary key constraint on columns "GenreId"
  """
  PK_Genre
}

"""
input type for incrementing numeric columns in table "Genre"
"""
input Genre_inc_input {
  GenreId: Int
}

"""
input type for inserting data into table "Genre"
"""
input Genre_insert_input {
  GenreId: Int
  Name: String
  Tracks: Track_arr_rel_insert_input
}

"""aggregate max on columns"""
type Genre_max_fields {
  GenreId: Int
  Name: String
}

"""aggregate min on columns"""
type Genre_min_fields {
  GenreId: Int
  Name: String
}

"""
response of any mutation on the table "Genre"
"""
type Genre_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Genre!]!
}

"""
input type for inserting object relation for remote table "Genre"
"""
input Genre_obj_rel_insert_input {
  data: Genre_insert_input!

  """upsert condition"""
  on_conflict: Genre_on_conflict
}

"""
on_conflict condition type for table "Genre"
"""
input Genre_on_conflict {
  constraint: Genre_constraint!
  update_columns: [Genre_update_column!]! = []
  where: Genre_bool_exp
}

"""Ordering options when selecting data from "Genre"."""
input Genre_order_by {
  GenreId: order_by
  Name: order_by
  Tracks_aggregate: Track_aggregate_order_by
}

"""primary key columns input for table: Genre"""
input Genre_pk_columns_input {
  GenreId: Int!
}

"""
select columns of table "Genre"
"""
enum Genre_select_column {
  """column name"""
  GenreId

  """column name"""
  Name
}

"""
input type for updating data in table "Genre"
"""
input Genre_set_input {
  GenreId: Int
  Name: String
}

"""aggregate stddev on columns"""
type Genre_stddev_fields {
  GenreId: Float
}

"""aggregate stddev_pop on columns"""
type Genre_stddev_pop_fields {
  GenreId: Float
}

"""aggregate stddev_samp on columns"""
type Genre_stddev_samp_fields {
  GenreId: Float
}

"""
Streaming cursor of the table "Genre"
"""
input Genre_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Genre_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Genre_stream_cursor_value_input {
  GenreId: Int
  Name: String
}

"""aggregate sum on columns"""
type Genre_sum_fields {
  GenreId: Int
}

"""
update columns of table "Genre"
"""
enum Genre_update_column {
  """column name"""
  GenreId

  """column name"""
  Name
}

input Genre_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Genre_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Genre_set_input

  """filter the rows which have to be updated"""
  where: Genre_bool_exp!
}

"""aggregate var_pop on columns"""
type Genre_var_pop_fields {
  GenreId: Float
}

"""aggregate var_samp on columns"""
type Genre_var_samp_fields {
  GenreId: Float
}

"""aggregate variance on columns"""
type Genre_variance_fields {
  GenreId: Float
}

"""
Boolean expression to compare columns of type "Int". All fields are combined with logical 'AND'.
"""
input Int_comparison_exp {
  _eq: Int
  _gt: Int
  _gte: Int
  _in: [Int!]
  _is_null: Boolean
  _lt: Int
  _lte: Int
  _neq: Int
  _nin: [Int!]
}

"""
columns and relationships of "Invoice"
"""
type Invoice {
  BillingAddress: String
  BillingCity: String
  BillingCountry: String
  BillingPostalCode: String
  BillingState: String

  """An object relationship"""
  Customer: Customer!
  CustomerId: Int!
  InvoiceDate: timestamp!
  InvoiceId: Int!

  """An array relationship"""
  InvoiceLines(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): [InvoiceLine!]!

  """An aggregate relationship"""
  InvoiceLines_aggregate(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): InvoiceLine_aggregate!
  Total: numeric!
}

"""
columns and relationships of "InvoiceLine"
"""
type InvoiceLine {
  """An object relationship"""
  Invoice: Invoice!
  InvoiceId: Int!
  InvoiceLineId: Int!
  Quantity: Int!

  """An object relationship"""
  Track: Track!
  TrackId: Int!
  UnitPrice: numeric!
}

"""
aggregated selection of "InvoiceLine"
"""
type InvoiceLine_aggregate {
  aggregate: InvoiceLine_aggregate_fields
  nodes: [InvoiceLine!]!
}

input InvoiceLine_aggregate_bool_exp {
  count: InvoiceLine_aggregate_bool_exp_count
}

input InvoiceLine_aggregate_bool_exp_count {
  arguments: [InvoiceLine_select_column!]
  distinct: Boolean
  filter: InvoiceLine_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "InvoiceLine"
"""
type InvoiceLine_aggregate_fields {
  avg: InvoiceLine_avg_fields
  count(columns: [InvoiceLine_select_column!], distinct: Boolean): Int!
  max: InvoiceLine_max_fields
  min: InvoiceLine_min_fields
  stddev: InvoiceLine_stddev_fields
  stddev_pop: InvoiceLine_stddev_pop_fields
  stddev_samp: InvoiceLine_stddev_samp_fields
  sum: InvoiceLine_sum_fields
  var_pop: InvoiceLine_var_pop_fields
  var_samp: InvoiceLine_var_samp_fields
  variance: InvoiceLine_variance_fields
}

"""
order by aggregate values of table "InvoiceLine"
"""
input InvoiceLine_aggregate_order_by {
  avg: InvoiceLine_avg_order_by
  count: order_by
  max: InvoiceLine_max_order_by
  min: InvoiceLine_min_order_by
  stddev: InvoiceLine_stddev_order_by
  stddev_pop: InvoiceLine_stddev_pop_order_by
  stddev_samp: InvoiceLine_stddev_samp_order_by
  sum: InvoiceLine_sum_order_by
  var_pop: InvoiceLine_var_pop_order_by
  var_samp: InvoiceLine_var_samp_order_by
  variance: InvoiceLine_variance_order_by
}

"""
input type for inserting array relation for remote table "InvoiceLine"
"""
input InvoiceLine_arr_rel_insert_input {
  data: [InvoiceLine_insert_input!]!

  """upsert condition"""
  on_conflict: InvoiceLine_on_conflict
}

"""aggregate avg on columns"""
type InvoiceLine_avg_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by avg() on columns of table "InvoiceLine"
"""
input InvoiceLine_avg_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
Boolean expression to filter rows from the table "InvoiceLine". All fields are combined with a logical 'AND'.
"""
input InvoiceLine_bool_exp {
  Invoice: Invoice_bool_exp
  InvoiceId: Int_comparison_exp
  InvoiceLineId: Int_comparison_exp
  Quantity: Int_comparison_exp
  Track: Track_bool_exp
  TrackId: Int_comparison_exp
  UnitPrice: numeric_comparison_exp
  _and: [InvoiceLine_bool_exp!]
  _not: InvoiceLine_bool_exp
  _or: [InvoiceLine_bool_exp!]
}

"""
unique or primary key constraints on table "InvoiceLine"
"""
enum InvoiceLine_constraint {
  """
  unique or primary key constraint on columns "InvoiceLineId"
  """
  PK_InvoiceLine
}

"""
input type for incrementing numeric columns in table "InvoiceLine"
"""
input InvoiceLine_inc_input {
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  TrackId: Int
  UnitPrice: numeric
}

"""
input type for inserting data into table "InvoiceLine"
"""
input InvoiceLine_insert_input {
  Invoice: Invoice_obj_rel_insert_input
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  Track: Track_obj_rel_insert_input
  TrackId: Int
  UnitPrice: numeric
}

"""aggregate max on columns"""
type InvoiceLine_max_fields {
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  TrackId: Int
  UnitPrice: numeric
}

"""
order by max() on columns of table "InvoiceLine"
"""
input InvoiceLine_max_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate min on columns"""
type InvoiceLine_min_fields {
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  TrackId: Int
  UnitPrice: numeric
}

"""
order by min() on columns of table "InvoiceLine"
"""
input InvoiceLine_min_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
response of any mutation on the table "InvoiceLine"
"""
type InvoiceLine_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [InvoiceLine!]!
}

"""
on_conflict condition type for table "InvoiceLine"
"""
input InvoiceLine_on_conflict {
  constraint: InvoiceLine_constraint!
  update_columns: [InvoiceLine_update_column!]! = []
  where: InvoiceLine_bool_exp
}

"""Ordering options when selecting data from "InvoiceLine"."""
input InvoiceLine_order_by {
  Invoice: Invoice_order_by
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  Track: Track_order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""primary key columns input for table: InvoiceLine"""
input InvoiceLine_pk_columns_input {
  InvoiceLineId: Int!
}

"""
select columns of table "InvoiceLine"
"""
enum InvoiceLine_select_column {
  """column name"""
  InvoiceId

  """column name"""
  InvoiceLineId

  """column name"""
  Quantity

  """column name"""
  TrackId

  """column name"""
  UnitPrice
}

"""
input type for updating data in table "InvoiceLine"
"""
input InvoiceLine_set_input {
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  TrackId: Int
  UnitPrice: numeric
}

"""aggregate stddev on columns"""
type InvoiceLine_stddev_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by stddev() on columns of table "InvoiceLine"
"""
input InvoiceLine_stddev_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate stddev_pop on columns"""
type InvoiceLine_stddev_pop_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by stddev_pop() on columns of table "InvoiceLine"
"""
input InvoiceLine_stddev_pop_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate stddev_samp on columns"""
type InvoiceLine_stddev_samp_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by stddev_samp() on columns of table "InvoiceLine"
"""
input InvoiceLine_stddev_samp_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
Streaming cursor of the table "InvoiceLine"
"""
input InvoiceLine_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: InvoiceLine_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input InvoiceLine_stream_cursor_value_input {
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  TrackId: Int
  UnitPrice: numeric
}

"""aggregate sum on columns"""
type InvoiceLine_sum_fields {
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  TrackId: Int
  UnitPrice: numeric
}

"""
order by sum() on columns of table "InvoiceLine"
"""
input InvoiceLine_sum_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
update columns of table "InvoiceLine"
"""
enum InvoiceLine_update_column {
  """column name"""
  InvoiceId

  """column name"""
  InvoiceLineId

  """column name"""
  Quantity

  """column name"""
  TrackId

  """column name"""
  UnitPrice
}

input InvoiceLine_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: InvoiceLine_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: InvoiceLine_set_input

  """filter the rows which have to be updated"""
  where: InvoiceLine_bool_exp!
}

"""aggregate var_pop on columns"""
type InvoiceLine_var_pop_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by var_pop() on columns of table "InvoiceLine"
"""
input InvoiceLine_var_pop_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate var_samp on columns"""
type InvoiceLine_var_samp_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by var_samp() on columns of table "InvoiceLine"
"""
input InvoiceLine_var_samp_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate variance on columns"""
type InvoiceLine_variance_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by variance() on columns of table "InvoiceLine"
"""
input InvoiceLine_variance_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
aggregated selection of "Invoice"
"""
type Invoice_aggregate {
  aggregate: Invoice_aggregate_fields
  nodes: [Invoice!]!
}

input Invoice_aggregate_bool_exp {
  count: Invoice_aggregate_bool_exp_count
}

input Invoice_aggregate_bool_exp_count {
  arguments: [Invoice_select_column!]
  distinct: Boolean
  filter: Invoice_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "Invoice"
"""
type Invoice_aggregate_fields {
  avg: Invoice_avg_fields
  count(columns: [Invoice_select_column!], distinct: Boolean): Int!
  max: Invoice_max_fields
  min: Invoice_min_fields
  stddev: Invoice_stddev_fields
  stddev_pop: Invoice_stddev_pop_fields
  stddev_samp: Invoice_stddev_samp_fields
  sum: Invoice_sum_fields
  var_pop: Invoice_var_pop_fields
  var_samp: Invoice_var_samp_fields
  variance: Invoice_variance_fields
}

"""
order by aggregate values of table "Invoice"
"""
input Invoice_aggregate_order_by {
  avg: Invoice_avg_order_by
  count: order_by
  max: Invoice_max_order_by
  min: Invoice_min_order_by
  stddev: Invoice_stddev_order_by
  stddev_pop: Invoice_stddev_pop_order_by
  stddev_samp: Invoice_stddev_samp_order_by
  sum: Invoice_sum_order_by
  var_pop: Invoice_var_pop_order_by
  var_samp: Invoice_var_samp_order_by
  variance: Invoice_variance_order_by
}

"""
input type for inserting array relation for remote table "Invoice"
"""
input Invoice_arr_rel_insert_input {
  data: [Invoice_insert_input!]!

  """upsert condition"""
  on_conflict: Invoice_on_conflict
}

"""aggregate avg on columns"""
type Invoice_avg_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by avg() on columns of table "Invoice"
"""
input Invoice_avg_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""
Boolean expression to filter rows from the table "Invoice". All fields are combined with a logical 'AND'.
"""
input Invoice_bool_exp {
  BillingAddress: String_comparison_exp
  BillingCity: String_comparison_exp
  BillingCountry: String_comparison_exp
  BillingPostalCode: String_comparison_exp
  BillingState: String_comparison_exp
  Customer: Customer_bool_exp
  CustomerId: Int_comparison_exp
  InvoiceDate: timestamp_comparison_exp
  InvoiceId: Int_comparison_exp
  InvoiceLines: InvoiceLine_bool_exp
  InvoiceLines_aggregate: InvoiceLine_aggregate_bool_exp
  Total: numeric_comparison_exp
  _and: [Invoice_bool_exp!]
  _not: Invoice_bool_exp
  _or: [Invoice_bool_exp!]
}

"""
unique or primary key constraints on table "Invoice"
"""
enum Invoice_constraint {
  """
  unique or primary key constraint on columns "InvoiceId"
  """
  PK_Invoice
}

"""
input type for incrementing numeric columns in table "Invoice"
"""
input Invoice_inc_input {
  CustomerId: Int
  InvoiceId: Int
  Total: numeric
}

"""
input type for inserting data into table "Invoice"
"""
input Invoice_insert_input {
  BillingAddress: String
  BillingCity: String
  BillingCountry: String
  BillingPostalCode: String
  BillingState: String
  Customer: Customer_obj_rel_insert_input
  CustomerId: Int
  InvoiceDate: timestamp
  InvoiceId: Int
  InvoiceLines: InvoiceLine_arr_rel_insert_input
  Total: numeric
}

"""aggregate max on columns"""
type Invoice_max_fields {
  BillingAddress: String
  BillingCity: String
  BillingCountry: String
  BillingPostalCode: String
  BillingState: String
  CustomerId: Int
  InvoiceDate: timestamp
  InvoiceId: Int
  Total: numeric
}

"""
order by max() on columns of table "Invoice"
"""
input Invoice_max_order_by {
  BillingAddress: order_by
  BillingCity: order_by
  BillingCountry: order_by
  BillingPostalCode: order_by
  BillingState: order_by
  CustomerId: order_by
  InvoiceDate: order_by
  InvoiceId: order_by
  Total: order_by
}

"""aggregate min on columns"""
type Invoice_min_fields {
  BillingAddress: String
  BillingCity: String
  BillingCountry: String
  BillingPostalCode: String
  BillingState: String
  CustomerId: Int
  InvoiceDate: timestamp
  InvoiceId: Int
  Total: numeric
}

"""
order by min() on columns of table "Invoice"
"""
input Invoice_min_order_by {
  BillingAddress: order_by
  BillingCity: order_by
  BillingCountry: order_by
  BillingPostalCode: order_by
  BillingState: order_by
  CustomerId: order_by
  InvoiceDate: order_by
  InvoiceId: order_by
  Total: order_by
}

"""
response of any mutation on the table "Invoice"
"""
type Invoice_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Invoice!]!
}

"""
input type for inserting object relation for remote table "Invoice"
"""
input Invoice_obj_rel_insert_input {
  data: Invoice_insert_input!

  """upsert condition"""
  on_conflict: Invoice_on_conflict
}

"""
on_conflict condition type for table "Invoice"
"""
input Invoice_on_conflict {
  constraint: Invoice_constraint!
  update_columns: [Invoice_update_column!]! = []
  where: Invoice_bool_exp
}

"""Ordering options when selecting data from "Invoice"."""
input Invoice_order_by {
  BillingAddress: order_by
  BillingCity: order_by
  BillingCountry: order_by
  BillingPostalCode: order_by
  BillingState: order_by
  Customer: Customer_order_by
  CustomerId: order_by
  InvoiceDate: order_by
  InvoiceId: order_by
  InvoiceLines_aggregate: InvoiceLine_aggregate_order_by
  Total: order_by
}

"""primary key columns input for table: Invoice"""
input Invoice_pk_columns_input {
  InvoiceId: Int!
}

"""
select columns of table "Invoice"
"""
enum Invoice_select_column {
  """column name"""
  BillingAddress

  """column name"""
  BillingCity

  """column name"""
  BillingCountry

  """column name"""
  BillingPostalCode

  """column name"""
  BillingState

  """column name"""
  CustomerId

  """column name"""
  InvoiceDate

  """column name"""
  InvoiceId

  """column name"""
  Total
}

"""
input type for updating data in table "Invoice"
"""
input Invoice_set_input {
  BillingAddress: String
  BillingCity: String
  BillingCountry: String
  BillingPostalCode: String
  BillingState: String
  CustomerId: Int
  InvoiceDate: timestamp
  InvoiceId: Int
  Total: numeric
}

"""aggregate stddev on columns"""
type Invoice_stddev_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by stddev() on columns of table "Invoice"
"""
input Invoice_stddev_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""aggregate stddev_pop on columns"""
type Invoice_stddev_pop_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by stddev_pop() on columns of table "Invoice"
"""
input Invoice_stddev_pop_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""aggregate stddev_samp on columns"""
type Invoice_stddev_samp_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by stddev_samp() on columns of table "Invoice"
"""
input Invoice_stddev_samp_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""
Streaming cursor of the table "Invoice"
"""
input Invoice_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Invoice_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Invoice_stream_cursor_value_input {
  BillingAddress: String
  BillingCity: String
  BillingCountry: String
  BillingPostalCode: String
  BillingState: String
  CustomerId: Int
  InvoiceDate: timestamp
  InvoiceId: Int
  Total: numeric
}

"""aggregate sum on columns"""
type Invoice_sum_fields {
  CustomerId: Int
  InvoiceId: Int
  Total: numeric
}

"""
order by sum() on columns of table "Invoice"
"""
input Invoice_sum_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""
update columns of table "Invoice"
"""
enum Invoice_update_column {
  """column name"""
  BillingAddress

  """column name"""
  BillingCity

  """column name"""
  BillingCountry

  """column name"""
  BillingPostalCode

  """column name"""
  BillingState

  """column name"""
  CustomerId

  """column name"""
  InvoiceDate

  """column name"""
  InvoiceId

  """column name"""
  Total
}

input Invoice_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Invoice_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Invoice_set_input

  """filter the rows which have to be updated"""
  where: Invoice_bool_exp!
}

"""aggregate var_pop on columns"""
type Invoice_var_pop_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by var_pop() on columns of table "Invoice"
"""
input Invoice_var_pop_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""aggregate var_samp on columns"""
type Invoice_var_samp_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by var_samp() on columns of table "Invoice"
"""
input Invoice_var_samp_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""aggregate variance on columns"""
type Invoice_variance_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by variance() on columns of table "Invoice"
"""
input Invoice_variance_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""
columns and relationships of "Listener"
"""
type Listener {
  country: String!
  dob: date!
  id: uuid!
  name: String!
}

"""
aggregated selection of "Listener"
"""
type Listener_aggregate {
  aggregate: Listener_aggregate_fields
  nodes: [Listener!]!
}

"""
aggregate fields of "Listener"
"""
type Listener_aggregate_fields {
  count(columns: [Listener_select_column!], distinct: Boolean): Int!
  max: Listener_max_fields
  min: Listener_min_fields
}

"""
Boolean expression to filter rows from the table "Listener". All fields are combined with a logical 'AND'.
"""
input Listener_bool_exp {
  _and: [Listener_bool_exp!]
  _not: Listener_bool_exp
  _or: [Listener_bool_exp!]
  country: String_comparison_exp
  dob: date_comparison_exp
  id: uuid_comparison_exp
  name: String_comparison_exp
}

"""
unique or primary key constraints on table "Listener"
"""
enum Listener_constraint {
  """
  unique or primary key constraint on columns "id"
  """
  Listener_pkey
}

"""
input type for inserting data into table "Listener"
"""
input Listener_insert_input {
  country: String
  dob: date
  id: uuid
  name: String
}

"""aggregate max on columns"""
type Listener_max_fields {
  country: String
  dob: date
  id: uuid
  name: String
}

"""aggregate min on columns"""
type Listener_min_fields {
  country: String
  dob: date
  id: uuid
  name: String
}

"""
response of any mutation on the table "Listener"
"""
type Listener_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Listener!]!
}

"""
input type for inserting object relation for remote table "Listener"
"""
input Listener_obj_rel_insert_input {
  data: Listener_insert_input!

  """upsert condition"""
  on_conflict: Listener_on_conflict
}

"""
on_conflict condition type for table "Listener"
"""
input Listener_on_conflict {
  constraint: Listener_constraint!
  update_columns: [Listener_update_column!]! = []
  where: Listener_bool_exp
}

"""Ordering options when selecting data from "Listener"."""
input Listener_order_by {
  country: order_by
  dob: order_by
  id: order_by
  name: order_by
}

"""primary key columns input for table: Listener"""
input Listener_pk_columns_input {
  id: uuid!
}

"""
select columns of table "Listener"
"""
enum Listener_select_column {
  """column name"""
  country

  """column name"""
  dob

  """column name"""
  id

  """column name"""
  name
}

"""
input type for updating data in table "Listener"
"""
input Listener_set_input {
  country: String
  dob: date
  id: uuid
  name: String
}

"""
Streaming cursor of the table "Listener"
"""
input Listener_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Listener_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Listener_stream_cursor_value_input {
  country: String
  dob: date
  id: uuid
  name: String
}

"""
update columns of table "Listener"
"""
enum Listener_update_column {
  """column name"""
  country

  """column name"""
  dob

  """column name"""
  id

  """column name"""
  name
}

input Listener_updates {
  """sets the columns of the filtered rows to the given values"""
  _set: Listener_set_input

  """filter the rows which have to be updated"""
  where: Listener_bool_exp!
}

"""
columns and relationships of "MediaType"
"""
type MediaType {
  MediaTypeId: Int!
  Name: String

  """An array relationship"""
  Tracks(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): [Track!]!

  """An aggregate relationship"""
  Tracks_aggregate(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): Track_aggregate!
}

"""
aggregated selection of "MediaType"
"""
type MediaType_aggregate {
  aggregate: MediaType_aggregate_fields
  nodes: [MediaType!]!
}

"""
aggregate fields of "MediaType"
"""
type MediaType_aggregate_fields {
  avg: MediaType_avg_fields
  count(columns: [MediaType_select_column!], distinct: Boolean): Int!
  max: MediaType_max_fields
  min: MediaType_min_fields
  stddev: MediaType_stddev_fields
  stddev_pop: MediaType_stddev_pop_fields
  stddev_samp: MediaType_stddev_samp_fields
  sum: MediaType_sum_fields
  var_pop: MediaType_var_pop_fields
  var_samp: MediaType_var_samp_fields
  variance: MediaType_variance_fields
}

"""aggregate avg on columns"""
type MediaType_avg_fields {
  MediaTypeId: Float
}

"""
Boolean expression to filter rows from the table "MediaType". All fields are combined with a logical 'AND'.
"""
input MediaType_bool_exp {
  MediaTypeId: Int_comparison_exp
  Name: String_comparison_exp
  Tracks: Track_bool_exp
  Tracks_aggregate: Track_aggregate_bool_exp
  _and: [MediaType_bool_exp!]
  _not: MediaType_bool_exp
  _or: [MediaType_bool_exp!]
}

"""
unique or primary key constraints on table "MediaType"
"""
enum MediaType_constraint {
  """
  unique or primary key constraint on columns "MediaTypeId"
  """
  PK_MediaType
}

"""
input type for incrementing numeric columns in table "MediaType"
"""
input MediaType_inc_input {
  MediaTypeId: Int
}

"""
input type for inserting data into table "MediaType"
"""
input MediaType_insert_input {
  MediaTypeId: Int
  Name: String
  Tracks: Track_arr_rel_insert_input
}

"""aggregate max on columns"""
type MediaType_max_fields {
  MediaTypeId: Int
  Name: String
}

"""aggregate min on columns"""
type MediaType_min_fields {
  MediaTypeId: Int
  Name: String
}

"""
response of any mutation on the table "MediaType"
"""
type MediaType_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [MediaType!]!
}

"""
input type for inserting object relation for remote table "MediaType"
"""
input MediaType_obj_rel_insert_input {
  data: MediaType_insert_input!

  """upsert condition"""
  on_conflict: MediaType_on_conflict
}

"""
on_conflict condition type for table "MediaType"
"""
input MediaType_on_conflict {
  constraint: MediaType_constraint!
  update_columns: [MediaType_update_column!]! = []
  where: MediaType_bool_exp
}

"""Ordering options when selecting data from "MediaType"."""
input MediaType_order_by {
  MediaTypeId: order_by
  Name: order_by
  Tracks_aggregate: Track_aggregate_order_by
}

"""primary key columns input for table: MediaType"""
input MediaType_pk_columns_input {
  MediaTypeId: Int!
}

"""
select columns of table "MediaType"
"""
enum MediaType_select_column {
  """column name"""
  MediaTypeId

  """column name"""
  Name
}

"""
input type for updating data in table "MediaType"
"""
input MediaType_set_input {
  MediaTypeId: Int
  Name: String
}

"""aggregate stddev on columns"""
type MediaType_stddev_fields {
  MediaTypeId: Float
}

"""aggregate stddev_pop on columns"""
type MediaType_stddev_pop_fields {
  MediaTypeId: Float
}

"""aggregate stddev_samp on columns"""
type MediaType_stddev_samp_fields {
  MediaTypeId: Float
}

"""
Streaming cursor of the table "MediaType"
"""
input MediaType_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: MediaType_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input MediaType_stream_cursor_value_input {
  MediaTypeId: Int
  Name: String
}

"""aggregate sum on columns"""
type MediaType_sum_fields {
  MediaTypeId: Int
}

"""
update columns of table "MediaType"
"""
enum MediaType_update_column {
  """column name"""
  MediaTypeId

  """column name"""
  Name
}

input MediaType_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: MediaType_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: MediaType_set_input

  """filter the rows which have to be updated"""
  where: MediaType_bool_exp!
}

"""aggregate var_pop on columns"""
type MediaType_var_pop_fields {
  MediaTypeId: Float
}

"""aggregate var_samp on columns"""
type MediaType_var_samp_fields {
  MediaTypeId: Float
}

"""aggregate variance on columns"""
type MediaType_variance_fields {
  MediaTypeId: Float
}

"""
columns and relationships of "Playlist"
"""
type Playlist {
  Name: String
  PlaylistId: Int!

  """An array relationship"""
  PlaylistTracks(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): [PlaylistTrack!]!

  """An aggregate relationship"""
  PlaylistTracks_aggregate(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): PlaylistTrack_aggregate!
}

"""
columns and relationships of "PlaylistTrack"
"""
type PlaylistTrack {
  """An object relationship"""
  Playlist: Playlist!
  PlaylistId: Int!

  """An object relationship"""
  Track: Track!
  TrackId: Int!
}

"""
aggregated selection of "PlaylistTrack"
"""
type PlaylistTrack_aggregate {
  aggregate: PlaylistTrack_aggregate_fields
  nodes: [PlaylistTrack!]!
}

input PlaylistTrack_aggregate_bool_exp {
  count: PlaylistTrack_aggregate_bool_exp_count
}

input PlaylistTrack_aggregate_bool_exp_count {
  arguments: [PlaylistTrack_select_column!]
  distinct: Boolean
  filter: PlaylistTrack_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "PlaylistTrack"
"""
type PlaylistTrack_aggregate_fields {
  avg: PlaylistTrack_avg_fields
  count(columns: [PlaylistTrack_select_column!], distinct: Boolean): Int!
  max: PlaylistTrack_max_fields
  min: PlaylistTrack_min_fields
  stddev: PlaylistTrack_stddev_fields
  stddev_pop: PlaylistTrack_stddev_pop_fields
  stddev_samp: PlaylistTrack_stddev_samp_fields
  sum: PlaylistTrack_sum_fields
  var_pop: PlaylistTrack_var_pop_fields
  var_samp: PlaylistTrack_var_samp_fields
  variance: PlaylistTrack_variance_fields
}

"""
order by aggregate values of table "PlaylistTrack"
"""
input PlaylistTrack_aggregate_order_by {
  avg: PlaylistTrack_avg_order_by
  count: order_by
  max: PlaylistTrack_max_order_by
  min: PlaylistTrack_min_order_by
  stddev: PlaylistTrack_stddev_order_by
  stddev_pop: PlaylistTrack_stddev_pop_order_by
  stddev_samp: PlaylistTrack_stddev_samp_order_by
  sum: PlaylistTrack_sum_order_by
  var_pop: PlaylistTrack_var_pop_order_by
  var_samp: PlaylistTrack_var_samp_order_by
  variance: PlaylistTrack_variance_order_by
}

"""
input type for inserting array relation for remote table "PlaylistTrack"
"""
input PlaylistTrack_arr_rel_insert_input {
  data: [PlaylistTrack_insert_input!]!

  """upsert condition"""
  on_conflict: PlaylistTrack_on_conflict
}

"""aggregate avg on columns"""
type PlaylistTrack_avg_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by avg() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_avg_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""
Boolean expression to filter rows from the table "PlaylistTrack". All fields are combined with a logical 'AND'.
"""
input PlaylistTrack_bool_exp {
  Playlist: Playlist_bool_exp
  PlaylistId: Int_comparison_exp
  Track: Track_bool_exp
  TrackId: Int_comparison_exp
  _and: [PlaylistTrack_bool_exp!]
  _not: PlaylistTrack_bool_exp
  _or: [PlaylistTrack_bool_exp!]
}

"""
unique or primary key constraints on table "PlaylistTrack"
"""
enum PlaylistTrack_constraint {
  """
  unique or primary key constraint on columns "PlaylistId", "TrackId"
  """
  PK_PlaylistTrack
}

"""
input type for incrementing numeric columns in table "PlaylistTrack"
"""
input PlaylistTrack_inc_input {
  PlaylistId: Int
  TrackId: Int
}

"""
input type for inserting data into table "PlaylistTrack"
"""
input PlaylistTrack_insert_input {
  Playlist: Playlist_obj_rel_insert_input
  PlaylistId: Int
  Track: Track_obj_rel_insert_input
  TrackId: Int
}

"""aggregate max on columns"""
type PlaylistTrack_max_fields {
  PlaylistId: Int
  TrackId: Int
}

"""
order by max() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_max_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""aggregate min on columns"""
type PlaylistTrack_min_fields {
  PlaylistId: Int
  TrackId: Int
}

"""
order by min() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_min_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""
response of any mutation on the table "PlaylistTrack"
"""
type PlaylistTrack_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [PlaylistTrack!]!
}

"""
on_conflict condition type for table "PlaylistTrack"
"""
input PlaylistTrack_on_conflict {
  constraint: PlaylistTrack_constraint!
  update_columns: [PlaylistTrack_update_column!]! = []
  where: PlaylistTrack_bool_exp
}

"""Ordering options when selecting data from "PlaylistTrack"."""
input PlaylistTrack_order_by {
  Playlist: Playlist_order_by
  PlaylistId: order_by
  Track: Track_order_by
  TrackId: order_by
}

"""primary key columns input for table: PlaylistTrack"""
input PlaylistTrack_pk_columns_input {
  PlaylistId: Int!
  TrackId: Int!
}

"""
select columns of table "PlaylistTrack"
"""
enum PlaylistTrack_select_column {
  """column name"""
  PlaylistId

  """column name"""
  TrackId
}

"""
input type for updating data in table "PlaylistTrack"
"""
input PlaylistTrack_set_input {
  PlaylistId: Int
  TrackId: Int
}

"""aggregate stddev on columns"""
type PlaylistTrack_stddev_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by stddev() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_stddev_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""aggregate stddev_pop on columns"""
type PlaylistTrack_stddev_pop_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by stddev_pop() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_stddev_pop_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""aggregate stddev_samp on columns"""
type PlaylistTrack_stddev_samp_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by stddev_samp() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_stddev_samp_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""
Streaming cursor of the table "PlaylistTrack"
"""
input PlaylistTrack_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: PlaylistTrack_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input PlaylistTrack_stream_cursor_value_input {
  PlaylistId: Int
  TrackId: Int
}

"""aggregate sum on columns"""
type PlaylistTrack_sum_fields {
  PlaylistId: Int
  TrackId: Int
}

"""
order by sum() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_sum_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""
update columns of table "PlaylistTrack"
"""
enum PlaylistTrack_update_column {
  """column name"""
  PlaylistId

  """column name"""
  TrackId
}

input PlaylistTrack_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: PlaylistTrack_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: PlaylistTrack_set_input

  """filter the rows which have to be updated"""
  where: PlaylistTrack_bool_exp!
}

"""aggregate var_pop on columns"""
type PlaylistTrack_var_pop_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by var_pop() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_var_pop_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""aggregate var_samp on columns"""
type PlaylistTrack_var_samp_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by var_samp() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_var_samp_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""aggregate variance on columns"""
type PlaylistTrack_variance_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by variance() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_variance_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""
aggregated selection of "Playlist"
"""
type Playlist_aggregate {
  aggregate: Playlist_aggregate_fields
  nodes: [Playlist!]!
}

"""
aggregate fields of "Playlist"
"""
type Playlist_aggregate_fields {
  avg: Playlist_avg_fields
  count(columns: [Playlist_select_column!], distinct: Boolean): Int!
  max: Playlist_max_fields
  min: Playlist_min_fields
  stddev: Playlist_stddev_fields
  stddev_pop: Playlist_stddev_pop_fields
  stddev_samp: Playlist_stddev_samp_fields
  sum: Playlist_sum_fields
  var_pop: Playlist_var_pop_fields
  var_samp: Playlist_var_samp_fields
  variance: Playlist_variance_fields
}

"""aggregate avg on columns"""
type Playlist_avg_fields {
  PlaylistId: Float
}

"""
Boolean expression to filter rows from the table "Playlist". All fields are combined with a logical 'AND'.
"""
input Playlist_bool_exp {
  Name: String_comparison_exp
  PlaylistId: Int_comparison_exp
  PlaylistTracks: PlaylistTrack_bool_exp
  PlaylistTracks_aggregate: PlaylistTrack_aggregate_bool_exp
  _and: [Playlist_bool_exp!]
  _not: Playlist_bool_exp
  _or: [Playlist_bool_exp!]
}

"""
unique or primary key constraints on table "Playlist"
"""
enum Playlist_constraint {
  """
  unique or primary key constraint on columns "PlaylistId"
  """
  PK_Playlist
}

"""
input type for incrementing numeric columns in table "Playlist"
"""
input Playlist_inc_input {
  PlaylistId: Int
}

"""
input type for inserting data into table "Playlist"
"""
input Playlist_insert_input {
  Name: String
  PlaylistId: Int
  PlaylistTracks: PlaylistTrack_arr_rel_insert_input
}

"""aggregate max on columns"""
type Playlist_max_fields {
  Name: String
  PlaylistId: Int
}

"""aggregate min on columns"""
type Playlist_min_fields {
  Name: String
  PlaylistId: Int
}

"""
response of any mutation on the table "Playlist"
"""
type Playlist_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Playlist!]!
}

"""
input type for inserting object relation for remote table "Playlist"
"""
input Playlist_obj_rel_insert_input {
  data: Playlist_insert_input!

  """upsert condition"""
  on_conflict: Playlist_on_conflict
}

"""
on_conflict condition type for table "Playlist"
"""
input Playlist_on_conflict {
  constraint: Playlist_constraint!
  update_columns: [Playlist_update_column!]! = []
  where: Playlist_bool_exp
}

"""Ordering options when selecting data from "Playlist"."""
input Playlist_order_by {
  Name: order_by
  PlaylistId: order_by
  PlaylistTracks_aggregate: PlaylistTrack_aggregate_order_by
}

"""primary key columns input for table: Playlist"""
input Playlist_pk_columns_input {
  PlaylistId: Int!
}

"""
select columns of table "Playlist"
"""
enum Playlist_select_column {
  """column name"""
  Name

  """column name"""
  PlaylistId
}

"""
input type for updating data in table "Playlist"
"""
input Playlist_set_input {
  Name: String
  PlaylistId: Int
}

"""aggregate stddev on columns"""
type Playlist_stddev_fields {
  PlaylistId: Float
}

"""aggregate stddev_pop on columns"""
type Playlist_stddev_pop_fields {
  PlaylistId: Float
}

"""aggregate stddev_samp on columns"""
type Playlist_stddev_samp_fields {
  PlaylistId: Float
}

"""
Streaming cursor of the table "Playlist"
"""
input Playlist_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Playlist_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Playlist_stream_cursor_value_input {
  Name: String
  PlaylistId: Int
}

"""aggregate sum on columns"""
type Playlist_sum_fields {
  PlaylistId: Int
}

"""
update columns of table "Playlist"
"""
enum Playlist_update_column {
  """column name"""
  Name

  """column name"""
  PlaylistId
}

input Playlist_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Playlist_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Playlist_set_input

  """filter the rows which have to be updated"""
  where: Playlist_bool_exp!
}

"""aggregate var_pop on columns"""
type Playlist_var_pop_fields {
  PlaylistId: Float
}

"""aggregate var_samp on columns"""
type Playlist_var_samp_fields {
  PlaylistId: Float
}

"""aggregate variance on columns"""
type Playlist_variance_fields {
  PlaylistId: Float
}

"""
Boolean expression to compare columns of type "String". All fields are combined with logical 'AND'.
"""
input String_comparison_exp {
  _eq: String
  _gt: String
  _gte: String

  """does the column match the given case-insensitive pattern"""
  _ilike: String
  _in: [String!]

  """
  does the column match the given POSIX regular expression, case insensitive
  """
  _iregex: String
  _is_null: Boolean

  """does the column match the given pattern"""
  _like: String
  _lt: String
  _lte: String
  _neq: String

  """does the column NOT match the given case-insensitive pattern"""
  _nilike: String
  _nin: [String!]

  """
  does the column NOT match the given POSIX regular expression, case insensitive
  """
  _niregex: String

  """does the column NOT match the given pattern"""
  _nlike: String

  """
  does the column NOT match the given POSIX regular expression, case sensitive
  """
  _nregex: String

  """does the column NOT match the given SQL regular expression"""
  _nsimilar: String

  """
  does the column match the given POSIX regular expression, case sensitive
  """
  _regex: String

  """does the column match the given SQL regular expression"""
  _similar: String
}

"""
columns and relationships of "Track"
"""
type Track {
  """An object relationship"""
  Album: Album
  AlbumId: Int
  Composer: String

  """An object relationship"""
  Genre: Genre
  GenreId: Int

  """An array relationship"""
  InvoiceLines(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): [InvoiceLine!]!

  """An aggregate relationship"""
  InvoiceLines_aggregate(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): InvoiceLine_aggregate!

  """An object relationship"""
  MediaType: MediaType!
  MediaTypeId: Int!
  Milliseconds: Int!
  Name: String!

  """An array relationship"""
  PlaylistTracks(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): [PlaylistTrack!]!

  """An aggregate relationship"""
  PlaylistTracks_aggregate(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): PlaylistTrack_aggregate!
  TrackId: Int!

  """An array relationship"""
  TrackPlays(
    """distinct select on columns"""
    distinct_on: [TrackPlay_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [TrackPlay_order_by!]

    """filter the rows returned"""
    where: TrackPlay_bool_exp
  ): [TrackPlay!]!

  """An aggregate relationship"""
  TrackPlays_aggregate(
    """distinct select on columns"""
    distinct_on: [TrackPlay_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [TrackPlay_order_by!]

    """filter the rows returned"""
    where: TrackPlay_bool_exp
  ): TrackPlay_aggregate!
  UnitPrice: numeric!
}

"""
columns and relationships of "TrackPlay"
"""
type TrackPlay {
  """An object relationship"""
  Listener: Listener!
  ListenerId: uuid!

  """An object relationship"""
  Track: Track!
  TrackId: Int!
  created_at: timestamptz!
  id: Int!
}

"""
aggregated selection of "TrackPlay"
"""
type TrackPlay_aggregate {
  aggregate: TrackPlay_aggregate_fields
  nodes: [TrackPlay!]!
}

input TrackPlay_aggregate_bool_exp {
  count: TrackPlay_aggregate_bool_exp_count
}

input TrackPlay_aggregate_bool_exp_count {
  arguments: [TrackPlay_select_column!]
  distinct: Boolean
  filter: TrackPlay_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "TrackPlay"
"""
type TrackPlay_aggregate_fields {
  avg: TrackPlay_avg_fields
  count(columns: [TrackPlay_select_column!], distinct: Boolean): Int!
  max: TrackPlay_max_fields
  min: TrackPlay_min_fields
  stddev: TrackPlay_stddev_fields
  stddev_pop: TrackPlay_stddev_pop_fields
  stddev_samp: TrackPlay_stddev_samp_fields
  sum: TrackPlay_sum_fields
  var_pop: TrackPlay_var_pop_fields
  var_samp: TrackPlay_var_samp_fields
  variance: TrackPlay_variance_fields
}

"""
order by aggregate values of table "TrackPlay"
"""
input TrackPlay_aggregate_order_by {
  avg: TrackPlay_avg_order_by
  count: order_by
  max: TrackPlay_max_order_by
  min: TrackPlay_min_order_by
  stddev: TrackPlay_stddev_order_by
  stddev_pop: TrackPlay_stddev_pop_order_by
  stddev_samp: TrackPlay_stddev_samp_order_by
  sum: TrackPlay_sum_order_by
  var_pop: TrackPlay_var_pop_order_by
  var_samp: TrackPlay_var_samp_order_by
  variance: TrackPlay_variance_order_by
}

"""
input type for inserting array relation for remote table "TrackPlay"
"""
input TrackPlay_arr_rel_insert_input {
  data: [TrackPlay_insert_input!]!

  """upsert condition"""
  on_conflict: TrackPlay_on_conflict
}

"""aggregate avg on columns"""
type TrackPlay_avg_fields {
  TrackId: Float
  id: Float
}

"""
order by avg() on columns of table "TrackPlay"
"""
input TrackPlay_avg_order_by {
  TrackId: order_by
  id: order_by
}

"""
Boolean expression to filter rows from the table "TrackPlay". All fields are combined with a logical 'AND'.
"""
input TrackPlay_bool_exp {
  Listener: Listener_bool_exp
  ListenerId: uuid_comparison_exp
  Track: Track_bool_exp
  TrackId: Int_comparison_exp
  _and: [TrackPlay_bool_exp!]
  _not: TrackPlay_bool_exp
  _or: [TrackPlay_bool_exp!]
  created_at: timestamptz_comparison_exp
  id: Int_comparison_exp
}

"""
unique or primary key constraints on table "TrackPlay"
"""
enum TrackPlay_constraint {
  """
  unique or primary key constraint on columns "id"
  """
  TrackPlay_pkey
}

"""
input type for incrementing numeric columns in table "TrackPlay"
"""
input TrackPlay_inc_input {
  TrackId: Int
  id: Int
}

"""
input type for inserting data into table "TrackPlay"
"""
input TrackPlay_insert_input {
  Listener: Listener_obj_rel_insert_input
  ListenerId: uuid
  Track: Track_obj_rel_insert_input
  TrackId: Int
  created_at: timestamptz
  id: Int
}

"""aggregate max on columns"""
type TrackPlay_max_fields {
  ListenerId: uuid
  TrackId: Int
  created_at: timestamptz
  id: Int
}

"""
order by max() on columns of table "TrackPlay"
"""
input TrackPlay_max_order_by {
  ListenerId: order_by
  TrackId: order_by
  created_at: order_by
  id: order_by
}

"""aggregate min on columns"""
type TrackPlay_min_fields {
  ListenerId: uuid
  TrackId: Int
  created_at: timestamptz
  id: Int
}

"""
order by min() on columns of table "TrackPlay"
"""
input TrackPlay_min_order_by {
  ListenerId: order_by
  TrackId: order_by
  created_at: order_by
  id: order_by
}

"""
response of any mutation on the table "TrackPlay"
"""
type TrackPlay_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [TrackPlay!]!
}

"""
on_conflict condition type for table "TrackPlay"
"""
input TrackPlay_on_conflict {
  constraint: TrackPlay_constraint!
  update_columns: [TrackPlay_update_column!]! = []
  where: TrackPlay_bool_exp
}

"""Ordering options when selecting data from "TrackPlay"."""
input TrackPlay_order_by {
  Listener: Listener_order_by
  ListenerId: order_by
  Track: Track_order_by
  TrackId: order_by
  created_at: order_by
  id: order_by
}

"""primary key columns input for table: TrackPlay"""
input TrackPlay_pk_columns_input {
  id: Int!
}

"""
select columns of table "TrackPlay"
"""
enum TrackPlay_select_column {
  """column name"""
  ListenerId

  """column name"""
  TrackId

  """column name"""
  created_at

  """column name"""
  id
}

"""
input type for updating data in table "TrackPlay"
"""
input TrackPlay_set_input {
  ListenerId: uuid
  TrackId: Int
  created_at: timestamptz
  id: Int
}

"""aggregate stddev on columns"""
type TrackPlay_stddev_fields {
  TrackId: Float
  id: Float
}

"""
order by stddev() on columns of table "TrackPlay"
"""
input TrackPlay_stddev_order_by {
  TrackId: order_by
  id: order_by
}

"""aggregate stddev_pop on columns"""
type TrackPlay_stddev_pop_fields {
  TrackId: Float
  id: Float
}

"""
order by stddev_pop() on columns of table "TrackPlay"
"""
input TrackPlay_stddev_pop_order_by {
  TrackId: order_by
  id: order_by
}

"""aggregate stddev_samp on columns"""
type TrackPlay_stddev_samp_fields {
  TrackId: Float
  id: Float
}

"""
order by stddev_samp() on columns of table "TrackPlay"
"""
input TrackPlay_stddev_samp_order_by {
  TrackId: order_by
  id: order_by
}

"""
Streaming cursor of the table "TrackPlay"
"""
input TrackPlay_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: TrackPlay_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input TrackPlay_stream_cursor_value_input {
  ListenerId: uuid
  TrackId: Int
  created_at: timestamptz
  id: Int
}

"""aggregate sum on columns"""
type TrackPlay_sum_fields {
  TrackId: Int
  id: Int
}

"""
order by sum() on columns of table "TrackPlay"
"""
input TrackPlay_sum_order_by {
  TrackId: order_by
  id: order_by
}

"""
update columns of table "TrackPlay"
"""
enum TrackPlay_update_column {
  """column name"""
  ListenerId

  """column name"""
  TrackId

  """column name"""
  created_at

  """column name"""
  id
}

input TrackPlay_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: TrackPlay_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: TrackPlay_set_input

  """filter the rows which have to be updated"""
  where: TrackPlay_bool_exp!
}

"""aggregate var_pop on columns"""
type TrackPlay_var_pop_fields {
  TrackId: Float
  id: Float
}

"""
order by var_pop() on columns of table "TrackPlay"
"""
input TrackPlay_var_pop_order_by {
  TrackId: order_by
  id: order_by
}

"""aggregate var_samp on columns"""
type TrackPlay_var_samp_fields {
  TrackId: Float
  id: Float
}

"""
order by var_samp() on columns of table "TrackPlay"
"""
input TrackPlay_var_samp_order_by {
  TrackId: order_by
  id: order_by
}

"""aggregate variance on columns"""
type TrackPlay_variance_fields {
  TrackId: Float
  id: Float
}

"""
order by variance() on columns of table "TrackPlay"
"""
input TrackPlay_variance_order_by {
  TrackId: order_by
  id: order_by
}

"""
aggregated selection of "Track"
"""
type Track_aggregate {
  aggregate: Track_aggregate_fields
  nodes: [Track!]!
}

input Track_aggregate_bool_exp {
  count: Track_aggregate_bool_exp_count
}

input Track_aggregate_bool_exp_count {
  arguments: [Track_select_column!]
  distinct: Boolean
  filter: Track_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "Track"
"""
type Track_aggregate_fields {
  avg: Track_avg_fields
  count(columns: [Track_select_column!], distinct: Boolean): Int!
  max: Track_max_fields
  min: Track_min_fields
  stddev: Track_stddev_fields
  stddev_pop: Track_stddev_pop_fields
  stddev_samp: Track_stddev_samp_fields
  sum: Track_sum_fields
  var_pop: Track_var_pop_fields
  var_samp: Track_var_samp_fields
  variance: Track_variance_fields
}

"""
order by aggregate values of table "Track"
"""
input Track_aggregate_order_by {
  avg: Track_avg_order_by
  count: order_by
  max: Track_max_order_by
  min: Track_min_order_by
  stddev: Track_stddev_order_by
  stddev_pop: Track_stddev_pop_order_by
  stddev_samp: Track_stddev_samp_order_by
  sum: Track_sum_order_by
  var_pop: Track_var_pop_order_by
  var_samp: Track_var_samp_order_by
  variance: Track_variance_order_by
}

"""
input type for inserting array relation for remote table "Track"
"""
input Track_arr_rel_insert_input {
  data: [Track_insert_input!]!

  """upsert condition"""
  on_conflict: Track_on_conflict
}

"""aggregate avg on columns"""
type Track_avg_fields {
  AlbumId: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by avg() on columns of table "Track"
"""
input Track_avg_order_by {
  AlbumId: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
Boolean expression to filter rows from the table "Track". All fields are combined with a logical 'AND'.
"""
input Track_bool_exp {
  Album: Album_bool_exp
  AlbumId: Int_comparison_exp
  Composer: String_comparison_exp
  Genre: Genre_bool_exp
  GenreId: Int_comparison_exp
  InvoiceLines: InvoiceLine_bool_exp
  InvoiceLines_aggregate: InvoiceLine_aggregate_bool_exp
  MediaType: MediaType_bool_exp
  MediaTypeId: Int_comparison_exp
  Milliseconds: Int_comparison_exp
  Name: String_comparison_exp
  PlaylistTracks: PlaylistTrack_bool_exp
  PlaylistTracks_aggregate: PlaylistTrack_aggregate_bool_exp
  TrackId: Int_comparison_exp
  TrackPlays: TrackPlay_bool_exp
  TrackPlays_aggregate: TrackPlay_aggregate_bool_exp
  UnitPrice: numeric_comparison_exp
  _and: [Track_bool_exp!]
  _not: Track_bool_exp
  _or: [Track_bool_exp!]
}

"""
unique or primary key constraints on table "Track"
"""
enum Track_constraint {
  """
  unique or primary key constraint on columns "TrackId"
  """
  PK_Track
}

"""
input type for incrementing numeric columns in table "Track"
"""
input Track_inc_input {
  AlbumId: Int
  GenreId: Int
  MediaTypeId: Int
  Milliseconds: Int
  TrackId: Int
  UnitPrice: numeric
}

"""
input type for inserting data into table "Track"
"""
input Track_insert_input {
  Album: Album_obj_rel_insert_input
  AlbumId: Int
  Composer: String
  Genre: Genre_obj_rel_insert_input
  GenreId: Int
  InvoiceLines: InvoiceLine_arr_rel_insert_input
  MediaType: MediaType_obj_rel_insert_input
  MediaTypeId: Int
  Milliseconds: Int
  Name: String
  PlaylistTracks: PlaylistTrack_arr_rel_insert_input
  TrackId: Int
  TrackPlays: TrackPlay_arr_rel_insert_input
  UnitPrice: numeric
}

"""aggregate max on columns"""
type Track_max_fields {
  AlbumId: Int
  Composer: String
  GenreId: Int
  MediaTypeId: Int
  Milliseconds: Int
  Name: String
  TrackId: Int
  UnitPrice: numeric
}

"""
order by max() on columns of table "Track"
"""
input Track_max_order_by {
  AlbumId: order_by
  Composer: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  Name: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate min on columns"""
type Track_min_fields {
  AlbumId: Int
  Composer: String
  GenreId: Int
  MediaTypeId: Int
  Milliseconds: Int
  Name: String
  TrackId: Int
  UnitPrice: numeric
}

"""
order by min() on columns of table "Track"
"""
input Track_min_order_by {
  AlbumId: order_by
  Composer: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  Name: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
response of any mutation on the table "Track"
"""
type Track_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Track!]!
}

"""
input type for inserting object relation for remote table "Track"
"""
input Track_obj_rel_insert_input {
  data: Track_insert_input!

  """upsert condition"""
  on_conflict: Track_on_conflict
}

"""
on_conflict condition type for table "Track"
"""
input Track_on_conflict {
  constraint: Track_constraint!
  update_columns: [Track_update_column!]! = []
  where: Track_bool_exp
}

"""Ordering options when selecting data from "Track"."""
input Track_order_by {
  Album: Album_order_by
  AlbumId: order_by
  Composer: order_by
  Genre: Genre_order_by
  GenreId: order_by
  InvoiceLines_aggregate: InvoiceLine_aggregate_order_by
  MediaType: MediaType_order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  Name: order_by
  PlaylistTracks_aggregate: PlaylistTrack_aggregate_order_by
  TrackId: order_by
  TrackPlays_aggregate: TrackPlay_aggregate_order_by
  UnitPrice: order_by
}

"""primary key columns input for table: Track"""
input Track_pk_columns_input {
  TrackId: Int!
}

"""
select columns of table "Track"
"""
enum Track_select_column {
  """column name"""
  AlbumId

  """column name"""
  Composer

  """column name"""
  GenreId

  """column name"""
  MediaTypeId

  """column name"""
  Milliseconds

  """column name"""
  Name

  """column name"""
  TrackId

  """column name"""
  UnitPrice
}

"""
input type for updating data in table "Track"
"""
input Track_set_input {
  AlbumId: Int
  Composer: String
  GenreId: Int
  MediaTypeId: Int
  Milliseconds: Int
  Name: String
  TrackId: Int
  UnitPrice: numeric
}

"""aggregate stddev on columns"""
type Track_stddev_fields {
  AlbumId: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by stddev() on columns of table "Track"
"""
input Track_stddev_order_by {
  AlbumId: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate stddev_pop on columns"""
type Track_stddev_pop_fields {
  AlbumId: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by stddev_pop() on columns of table "Track"
"""
input Track_stddev_pop_order_by {
  AlbumId: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate stddev_samp on columns"""
type Track_stddev_samp_fields {
  AlbumId: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by stddev_samp() on columns of table "Track"
"""
input Track_stddev_samp_order_by {
  AlbumId: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
Streaming cursor of the table "Track"
"""
input Track_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Track_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Track_stream_cursor_value_input {
  AlbumId: Int
  Composer: String
  GenreId: Int
  MediaTypeId: Int
  Milliseconds: Int
  Name: String
  TrackId: Int
  UnitPrice: numeric
}

"""aggregate sum on columns"""
type Track_sum_fields {
  AlbumId: Int
  GenreId: Int
  MediaTypeId: Int
  Milliseconds: Int
  TrackId: Int
  UnitPrice: numeric
}

"""
order by sum() on columns of table "Track"
"""
input Track_sum_order_by {
  AlbumId: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
update columns of table "Track"
"""
enum Track_update_column {
  """column name"""
  AlbumId

  """column name"""
  Composer

  """column name"""
  GenreId

  """column name"""
  MediaTypeId

  """column name"""
  Milliseconds

  """column name"""
  Name

  """column name"""
  TrackId

  """column name"""
  UnitPrice
}

input Track_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Track_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Track_set_input

  """filter the rows which have to be updated"""
  where: Track_bool_exp!
}

"""aggregate var_pop on columns"""
type Track_var_pop_fields {
  AlbumId: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by var_pop() on columns of table "Track"
"""
input Track_var_pop_order_by {
  AlbumId: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate var_samp on columns"""
type Track_var_samp_fields {
  AlbumId: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by var_samp() on columns of table "Track"
"""
input Track_var_samp_order_by {
  AlbumId: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate variance on columns"""
type Track_variance_fields {
  AlbumId: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by variance() on columns of table "Track"
"""
input Track_variance_order_by {
  AlbumId: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""ordering argument of a cursor"""
enum cursor_ordering {
  """ascending ordering of the cursor"""
  ASC

  """descending ordering of the cursor"""
  DESC
}

scalar date

"""
Boolean expression to compare columns of type "date". All fields are combined with logical 'AND'.
"""
input date_comparison_exp {
  _eq: date
  _gt: date
  _gte: date
  _in: [date!]
  _is_null: Boolean
  _lt: date
  _lte: date
  _neq: date
  _nin: [date!]
}

"""mutation root"""
type mutation_root {
  """
  delete data from the table: "Album"
  """
  delete_Album(
    """filter the rows which have to be deleted"""
    where: Album_bool_exp!
  ): Album_mutation_response

  """
  delete single row from the table: "Album"
  """
  delete_Album_by_pk(AlbumId: Int!): Album

  """
  delete data from the table: "Artist"
  """
  delete_Artist(
    """filter the rows which have to be deleted"""
    where: Artist_bool_exp!
  ): Artist_mutation_response

  """
  delete single row from the table: "Artist"
  """
  delete_Artist_by_pk(ArtistId: Int!): Artist

  """
  delete data from the table: "Customer"
  """
  delete_Customer(
    """filter the rows which have to be deleted"""
    where: Customer_bool_exp!
  ): Customer_mutation_response

  """
  delete single row from the table: "Customer"
  """
  delete_Customer_by_pk(CustomerId: Int!): Customer

  """
  delete data from the table: "Employee"
  """
  delete_Employee(
    """filter the rows which have to be deleted"""
    where: Employee_bool_exp!
  ): Employee_mutation_response

  """
  delete single row from the table: "Employee"
  """
  delete_Employee_by_pk(EmployeeId: Int!): Employee

  """
  delete data from the table: "Genre"
  """
  delete_Genre(
    """filter the rows which have to be deleted"""
    where: Genre_bool_exp!
  ): Genre_mutation_response

  """
  delete single row from the table: "Genre"
  """
  delete_Genre_by_pk(GenreId: Int!): Genre

  """
  delete data from the table: "Invoice"
  """
  delete_Invoice(
    """filter the rows which have to be deleted"""
    where: Invoice_bool_exp!
  ): Invoice_mutation_response

  """
  delete data from the table: "InvoiceLine"
  """
  delete_InvoiceLine(
    """filter the rows which have to be deleted"""
    where: InvoiceLine_bool_exp!
  ): InvoiceLine_mutation_response

  """
  delete single row from the table: "InvoiceLine"
  """
  delete_InvoiceLine_by_pk(InvoiceLineId: Int!): InvoiceLine

  """
  delete single row from the table: "Invoice"
  """
  delete_Invoice_by_pk(InvoiceId: Int!): Invoice

  """
  delete data from the table: "Listener"
  """
  delete_Listener(
    """filter the rows which have to be deleted"""
    where: Listener_bool_exp!
  ): Listener_mutation_response

  """
  delete single row from the table: "Listener"
  """
  delete_Listener_by_pk(id: uuid!): Listener

  """
  delete data from the table: "MediaType"
  """
  delete_MediaType(
    """filter the rows which have to be deleted"""
    where: MediaType_bool_exp!
  ): MediaType_mutation_response

  """
  delete single row from the table: "MediaType"
  """
  delete_MediaType_by_pk(MediaTypeId: Int!): MediaType

  """
  delete data from the table: "Playlist"
  """
  delete_Playlist(
    """filter the rows which have to be deleted"""
    where: Playlist_bool_exp!
  ): Playlist_mutation_response

  """
  delete data from the table: "PlaylistTrack"
  """
  delete_PlaylistTrack(
    """filter the rows which have to be deleted"""
    where: PlaylistTrack_bool_exp!
  ): PlaylistTrack_mutation_response

  """
  delete single row from the table: "PlaylistTrack"
  """
  delete_PlaylistTrack_by_pk(PlaylistId: Int!, TrackId: Int!): PlaylistTrack

  """
  delete single row from the table: "Playlist"
  """
  delete_Playlist_by_pk(PlaylistId: Int!): Playlist

  """
  delete data from the table: "Track"
  """
  delete_Track(
    """filter the rows which have to be deleted"""
    where: Track_bool_exp!
  ): Track_mutation_response

  """
  delete data from the table: "TrackPlay"
  """
  delete_TrackPlay(
    """filter the rows which have to be deleted"""
    where: TrackPlay_bool_exp!
  ): TrackPlay_mutation_response

  """
  delete single row from the table: "TrackPlay"
  """
  delete_TrackPlay_by_pk(id: Int!): TrackPlay

  """
  delete single row from the table: "Track"
  """
  delete_Track_by_pk(TrackId: Int!): Track

  """
  insert data into the table: "Album"
  """
  insert_Album(
    """the rows to be inserted"""
    objects: [Album_insert_input!]!

    """upsert condition"""
    on_conflict: Album_on_conflict
  ): Album_mutation_response

  """
  insert a single row into the table: "Album"
  """
  insert_Album_one(
    """the row to be inserted"""
    object: Album_insert_input!

    """upsert condition"""
    on_conflict: Album_on_conflict
  ): Album

  """
  insert data into the table: "Artist"
  """
  insert_Artist(
    """the rows to be inserted"""
    objects: [Artist_insert_input!]!

    """upsert condition"""
    on_conflict: Artist_on_conflict
  ): Artist_mutation_response

  """
  insert a single row into the table: "Artist"
  """
  insert_Artist_one(
    """the row to be inserted"""
    object: Artist_insert_input!

    """upsert condition"""
    on_conflict: Artist_on_conflict
  ): Artist

  """
  insert data into the table: "Customer"
  """
  insert_Customer(
    """the rows to be inserted"""
    objects: [Customer_insert_input!]!

    """upsert condition"""
    on_conflict: Customer_on_conflict
  ): Customer_mutation_response

  """
  insert a single row into the table: "Customer"
  """
  insert_Customer_one(
    """the row to be inserted"""
    object: Customer_insert_input!

    """upsert condition"""
    on_conflict: Customer_on_conflict
  ): Customer

  """
  insert data into the table: "Employee"
  """
  insert_Employee(
    """the rows to be inserted"""
    objects: [Employee_insert_input!]!

    """upsert condition"""
    on_conflict: Employee_on_conflict
  ): Employee_mutation_response

  """
  insert a single row into the table: "Employee"
  """
  insert_Employee_one(
    """the row to be inserted"""
    object: Employee_insert_input!

    """upsert condition"""
    on_conflict: Employee_on_conflict
  ): Employee

  """
  insert data into the table: "Genre"
  """
  insert_Genre(
    """the rows to be inserted"""
    objects: [Genre_insert_input!]!

    """upsert condition"""
    on_conflict: Genre_on_conflict
  ): Genre_mutation_response

  """
  insert a single row into the table: "Genre"
  """
  insert_Genre_one(
    """the row to be inserted"""
    object: Genre_insert_input!

    """upsert condition"""
    on_conflict: Genre_on_conflict
  ): Genre

  """
  insert data into the table: "Invoice"
  """
  insert_Invoice(
    """the rows to be inserted"""
    objects: [Invoice_insert_input!]!

    """upsert condition"""
    on_conflict: Invoice_on_conflict
  ): Invoice_mutation_response

  """
  insert data into the table: "InvoiceLine"
  """
  insert_InvoiceLine(
    """the rows to be inserted"""
    objects: [InvoiceLine_insert_input!]!

    """upsert condition"""
    on_conflict: InvoiceLine_on_conflict
  ): InvoiceLine_mutation_response

  """
  insert a single row into the table: "InvoiceLine"
  """
  insert_InvoiceLine_one(
    """the row to be inserted"""
    object: InvoiceLine_insert_input!

    """upsert condition"""
    on_conflict: InvoiceLine_on_conflict
  ): InvoiceLine

  """
  insert a single row into the table: "Invoice"
  """
  insert_Invoice_one(
    """the row to be inserted"""
    object: Invoice_insert_input!

    """upsert condition"""
    on_conflict: Invoice_on_conflict
  ): Invoice

  """
  insert data into the table: "Listener"
  """
  insert_Listener(
    """the rows to be inserted"""
    objects: [Listener_insert_input!]!

    """upsert condition"""
    on_conflict: Listener_on_conflict
  ): Listener_mutation_response

  """
  insert a single row into the table: "Listener"
  """
  insert_Listener_one(
    """the row to be inserted"""
    object: Listener_insert_input!

    """upsert condition"""
    on_conflict: Listener_on_conflict
  ): Listener

  """
  insert data into the table: "MediaType"
  """
  insert_MediaType(
    """the rows to be inserted"""
    objects: [MediaType_insert_input!]!

    """upsert condition"""
    on_conflict: MediaType_on_conflict
  ): MediaType_mutation_response

  """
  insert a single row into the table: "MediaType"
  """
  insert_MediaType_one(
    """the row to be inserted"""
    object: MediaType_insert_input!

    """upsert condition"""
    on_conflict: MediaType_on_conflict
  ): MediaType

  """
  insert data into the table: "Playlist"
  """
  insert_Playlist(
    """the rows to be inserted"""
    objects: [Playlist_insert_input!]!

    """upsert condition"""
    on_conflict: Playlist_on_conflict
  ): Playlist_mutation_response

  """
  insert data into the table: "PlaylistTrack"
  """
  insert_PlaylistTrack(
    """the rows to be inserted"""
    objects: [PlaylistTrack_insert_input!]!

    """upsert condition"""
    on_conflict: PlaylistTrack_on_conflict
  ): PlaylistTrack_mutation_response

  """
  insert a single row into the table: "PlaylistTrack"
  """
  insert_PlaylistTrack_one(
    """the row to be inserted"""
    object: PlaylistTrack_insert_input!

    """upsert condition"""
    on_conflict: PlaylistTrack_on_conflict
  ): PlaylistTrack

  """
  insert a single row into the table: "Playlist"
  """
  insert_Playlist_one(
    """the row to be inserted"""
    object: Playlist_insert_input!

    """upsert condition"""
    on_conflict: Playlist_on_conflict
  ): Playlist

  """
  insert data into the table: "Track"
  """
  insert_Track(
    """the rows to be inserted"""
    objects: [Track_insert_input!]!

    """upsert condition"""
    on_conflict: Track_on_conflict
  ): Track_mutation_response

  """
  insert data into the table: "TrackPlay"
  """
  insert_TrackPlay(
    """the rows to be inserted"""
    objects: [TrackPlay_insert_input!]!

    """upsert condition"""
    on_conflict: TrackPlay_on_conflict
  ): TrackPlay_mutation_response

  """
  insert a single row into the table: "TrackPlay"
  """
  insert_TrackPlay_one(
    """the row to be inserted"""
    object: TrackPlay_insert_input!

    """upsert condition"""
    on_conflict: TrackPlay_on_conflict
  ): TrackPlay

  """
  insert a single row into the table: "Track"
  """
  insert_Track_one(
    """the row to be inserted"""
    object: Track_insert_input!

    """upsert condition"""
    on_conflict: Track_on_conflict
  ): Track

  """
  update data of the table: "Album"
  """
  update_Album(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Album_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Album_set_input

    """filter the rows which have to be updated"""
    where: Album_bool_exp!
  ): Album_mutation_response

  """
  update single row of the table: "Album"
  """
  update_Album_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Album_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Album_set_input
    pk_columns: Album_pk_columns_input!
  ): Album

  """
  update multiples rows of table: "Album"
  """
  update_Album_many(
    """updates to execute, in order"""
    updates: [Album_updates!]!
  ): [Album_mutation_response]

  """
  update data of the table: "Artist"
  """
  update_Artist(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Artist_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Artist_set_input

    """filter the rows which have to be updated"""
    where: Artist_bool_exp!
  ): Artist_mutation_response

  """
  update single row of the table: "Artist"
  """
  update_Artist_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Artist_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Artist_set_input
    pk_columns: Artist_pk_columns_input!
  ): Artist

  """
  update multiples rows of table: "Artist"
  """
  update_Artist_many(
    """updates to execute, in order"""
    updates: [Artist_updates!]!
  ): [Artist_mutation_response]

  """
  update data of the table: "Customer"
  """
  update_Customer(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Customer_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Customer_set_input

    """filter the rows which have to be updated"""
    where: Customer_bool_exp!
  ): Customer_mutation_response

  """
  update single row of the table: "Customer"
  """
  update_Customer_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Customer_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Customer_set_input
    pk_columns: Customer_pk_columns_input!
  ): Customer

  """
  update multiples rows of table: "Customer"
  """
  update_Customer_many(
    """updates to execute, in order"""
    updates: [Customer_updates!]!
  ): [Customer_mutation_response]

  """
  update data of the table: "Employee"
  """
  update_Employee(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Employee_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Employee_set_input

    """filter the rows which have to be updated"""
    where: Employee_bool_exp!
  ): Employee_mutation_response

  """
  update single row of the table: "Employee"
  """
  update_Employee_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Employee_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Employee_set_input
    pk_columns: Employee_pk_columns_input!
  ): Employee

  """
  update multiples rows of table: "Employee"
  """
  update_Employee_many(
    """updates to execute, in order"""
    updates: [Employee_updates!]!
  ): [Employee_mutation_response]

  """
  update data of the table: "Genre"
  """
  update_Genre(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Genre_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Genre_set_input

    """filter the rows which have to be updated"""
    where: Genre_bool_exp!
  ): Genre_mutation_response

  """
  update single row of the table: "Genre"
  """
  update_Genre_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Genre_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Genre_set_input
    pk_columns: Genre_pk_columns_input!
  ): Genre

  """
  update multiples rows of table: "Genre"
  """
  update_Genre_many(
    """updates to execute, in order"""
    updates: [Genre_updates!]!
  ): [Genre_mutation_response]

  """
  update data of the table: "Invoice"
  """
  update_Invoice(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Invoice_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Invoice_set_input

    """filter the rows which have to be updated"""
    where: Invoice_bool_exp!
  ): Invoice_mutation_response

  """
  update data of the table: "InvoiceLine"
  """
  update_InvoiceLine(
    """increments the numeric columns with given value of the filtered values"""
    _inc: InvoiceLine_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: InvoiceLine_set_input

    """filter the rows which have to be updated"""
    where: InvoiceLine_bool_exp!
  ): InvoiceLine_mutation_response

  """
  update single row of the table: "InvoiceLine"
  """
  update_InvoiceLine_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: InvoiceLine_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: InvoiceLine_set_input
    pk_columns: InvoiceLine_pk_columns_input!
  ): InvoiceLine

  """
  update multiples rows of table: "InvoiceLine"
  """
  update_InvoiceLine_many(
    """updates to execute, in order"""
    updates: [InvoiceLine_updates!]!
  ): [InvoiceLine_mutation_response]

  """
  update single row of the table: "Invoice"
  """
  update_Invoice_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Invoice_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Invoice_set_input
    pk_columns: Invoice_pk_columns_input!
  ): Invoice

  """
  update multiples rows of table: "Invoice"
  """
  update_Invoice_many(
    """updates to execute, in order"""
    updates: [Invoice_updates!]!
  ): [Invoice_mutation_response]

  """
  update data of the table: "Listener"
  """
  update_Listener(
    """sets the columns of the filtered rows to the given values"""
    _set: Listener_set_input

    """filter the rows which have to be updated"""
    where: Listener_bool_exp!
  ): Listener_mutation_response

  """
  update single row of the table: "Listener"
  """
  update_Listener_by_pk(
    """sets the columns of the filtered rows to the given values"""
    _set: Listener_set_input
    pk_columns: Listener_pk_columns_input!
  ): Listener

  """
  update multiples rows of table: "Listener"
  """
  update_Listener_many(
    """updates to execute, in order"""
    updates: [Listener_updates!]!
  ): [Listener_mutation_response]

  """
  update data of the table: "MediaType"
  """
  update_MediaType(
    """increments the numeric columns with given value of the filtered values"""
    _inc: MediaType_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: MediaType_set_input

    """filter the rows which have to be updated"""
    where: MediaType_bool_exp!
  ): MediaType_mutation_response

  """
  update single row of the table: "MediaType"
  """
  update_MediaType_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: MediaType_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: MediaType_set_input
    pk_columns: MediaType_pk_columns_input!
  ): MediaType

  """
  update multiples rows of table: "MediaType"
  """
  update_MediaType_many(
    """updates to execute, in order"""
    updates: [MediaType_updates!]!
  ): [MediaType_mutation_response]

  """
  update data of the table: "Playlist"
  """
  update_Playlist(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Playlist_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Playlist_set_input

    """filter the rows which have to be updated"""
    where: Playlist_bool_exp!
  ): Playlist_mutation_response

  """
  update data of the table: "PlaylistTrack"
  """
  update_PlaylistTrack(
    """increments the numeric columns with given value of the filtered values"""
    _inc: PlaylistTrack_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: PlaylistTrack_set_input

    """filter the rows which have to be updated"""
    where: PlaylistTrack_bool_exp!
  ): PlaylistTrack_mutation_response

  """
  update single row of the table: "PlaylistTrack"
  """
  update_PlaylistTrack_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: PlaylistTrack_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: PlaylistTrack_set_input
    pk_columns: PlaylistTrack_pk_columns_input!
  ): PlaylistTrack

  """
  update multiples rows of table: "PlaylistTrack"
  """
  update_PlaylistTrack_many(
    """updates to execute, in order"""
    updates: [PlaylistTrack_updates!]!
  ): [PlaylistTrack_mutation_response]

  """
  update single row of the table: "Playlist"
  """
  update_Playlist_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Playlist_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Playlist_set_input
    pk_columns: Playlist_pk_columns_input!
  ): Playlist

  """
  update multiples rows of table: "Playlist"
  """
  update_Playlist_many(
    """updates to execute, in order"""
    updates: [Playlist_updates!]!
  ): [Playlist_mutation_response]

  """
  update data of the table: "Track"
  """
  update_Track(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Track_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Track_set_input

    """filter the rows which have to be updated"""
    where: Track_bool_exp!
  ): Track_mutation_response

  """
  update data of the table: "TrackPlay"
  """
  update_TrackPlay(
    """increments the numeric columns with given value of the filtered values"""
    _inc: TrackPlay_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: TrackPlay_set_input

    """filter the rows which have to be updated"""
    where: TrackPlay_bool_exp!
  ): TrackPlay_mutation_response

  """
  update single row of the table: "TrackPlay"
  """
  update_TrackPlay_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: TrackPlay_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: TrackPlay_set_input
    pk_columns: TrackPlay_pk_columns_input!
  ): TrackPlay

  """
  update multiples rows of table: "TrackPlay"
  """
  update_TrackPlay_many(
    """updates to execute, in order"""
    updates: [TrackPlay_updates!]!
  ): [TrackPlay_mutation_response]

  """
  update single row of the table: "Track"
  """
  update_Track_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Track_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Track_set_input
    pk_columns: Track_pk_columns_input!
  ): Track

  """
  update multiples rows of table: "Track"
  """
  update_Track_many(
    """updates to execute, in order"""
    updates: [Track_updates!]!
  ): [Track_mutation_response]
}

scalar numeric

"""
Boolean expression to compare columns of type "numeric". All fields are combined with logical 'AND'.
"""
input numeric_comparison_exp {
  _eq: numeric
  _gt: numeric
  _gte: numeric
  _in: [numeric!]
  _is_null: Boolean
  _lt: numeric
  _lte: numeric
  _neq: numeric
  _nin: [numeric!]
}

"""column ordering options"""
enum order_by {
  """in ascending order, nulls last"""
  asc

  """in ascending order, nulls first"""
  asc_nulls_first

  """in ascending order, nulls last"""
  asc_nulls_last

  """in descending order, nulls first"""
  desc

  """in descending order, nulls first"""
  desc_nulls_first

  """in descending order, nulls last"""
  desc_nulls_last
}

type query_root {
  """
  fetch data from the table: "Album"
  """
  Album(
    """distinct select on columns"""
    distinct_on: [Album_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Album_order_by!]

    """filter the rows returned"""
    where: Album_bool_exp
  ): [Album!]!

  """
  fetch aggregated fields from the table: "Album"
  """
  Album_aggregate(
    """distinct select on columns"""
    distinct_on: [Album_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Album_order_by!]

    """filter the rows returned"""
    where: Album_bool_exp
  ): Album_aggregate!

  """fetch data from the table: "Album" using primary key columns"""
  Album_by_pk(AlbumId: Int!): Album

  """
  fetch data from the table: "Artist"
  """
  Artist(
    """distinct select on columns"""
    distinct_on: [Artist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Artist_order_by!]

    """filter the rows returned"""
    where: Artist_bool_exp
  ): [Artist!]!

  """
  fetch aggregated fields from the table: "Artist"
  """
  Artist_aggregate(
    """distinct select on columns"""
    distinct_on: [Artist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Artist_order_by!]

    """filter the rows returned"""
    where: Artist_bool_exp
  ): Artist_aggregate!

  """fetch data from the table: "Artist" using primary key columns"""
  Artist_by_pk(ArtistId: Int!): Artist

  """
  fetch data from the table: "Customer"
  """
  Customer(
    """distinct select on columns"""
    distinct_on: [Customer_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Customer_order_by!]

    """filter the rows returned"""
    where: Customer_bool_exp
  ): [Customer!]!

  """
  fetch aggregated fields from the table: "Customer"
  """
  Customer_aggregate(
    """distinct select on columns"""
    distinct_on: [Customer_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Customer_order_by!]

    """filter the rows returned"""
    where: Customer_bool_exp
  ): Customer_aggregate!

  """fetch data from the table: "Customer" using primary key columns"""
  Customer_by_pk(CustomerId: Int!): Customer

  """
  fetch data from the table: "Employee"
  """
  Employee(
    """distinct select on columns"""
    distinct_on: [Employee_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Employee_order_by!]

    """filter the rows returned"""
    where: Employee_bool_exp
  ): [Employee!]!

  """
  fetch aggregated fields from the table: "Employee"
  """
  Employee_aggregate(
    """distinct select on columns"""
    distinct_on: [Employee_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Employee_order_by!]

    """filter the rows returned"""
    where: Employee_bool_exp
  ): Employee_aggregate!

  """fetch data from the table: "Employee" using primary key columns"""
  Employee_by_pk(EmployeeId: Int!): Employee

  """
  fetch data from the table: "Genre"
  """
  Genre(
    """distinct select on columns"""
    distinct_on: [Genre_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Genre_order_by!]

    """filter the rows returned"""
    where: Genre_bool_exp
  ): [Genre!]!

  """
  fetch aggregated fields from the table: "Genre"
  """
  Genre_aggregate(
    """distinct select on columns"""
    distinct_on: [Genre_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Genre_order_by!]

    """filter the rows returned"""
    where: Genre_bool_exp
  ): Genre_aggregate!

  """fetch data from the table: "Genre" using primary key columns"""
  Genre_by_pk(GenreId: Int!): Genre

  """
  fetch data from the table: "Invoice"
  """
  Invoice(
    """distinct select on columns"""
    distinct_on: [Invoice_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Invoice_order_by!]

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): [Invoice!]!

  """
  fetch data from the table: "InvoiceLine"
  """
  InvoiceLine(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): [InvoiceLine!]!

  """
  fetch aggregated fields from the table: "InvoiceLine"
  """
  InvoiceLine_aggregate(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): InvoiceLine_aggregate!

  """fetch data from the table: "InvoiceLine" using primary key columns"""
  InvoiceLine_by_pk(InvoiceLineId: Int!): InvoiceLine

  """
  fetch aggregated fields from the table: "Invoice"
  """
  Invoice_aggregate(
    """distinct select on columns"""
    distinct_on: [Invoice_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Invoice_order_by!]

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): Invoice_aggregate!

  """fetch data from the table: "Invoice" using primary key columns"""
  Invoice_by_pk(InvoiceId: Int!): Invoice

  """
  fetch data from the table: "Listener"
  """
  Listener(
    """distinct select on columns"""
    distinct_on: [Listener_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Listener_order_by!]

    """filter the rows returned"""
    where: Listener_bool_exp
  ): [Listener!]!

  """
  fetch aggregated fields from the table: "Listener"
  """
  Listener_aggregate(
    """distinct select on columns"""
    distinct_on: [Listener_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Listener_order_by!]

    """filter the rows returned"""
    where: Listener_bool_exp
  ): Listener_aggregate!

  """fetch data from the table: "Listener" using primary key columns"""
  Listener_by_pk(id: uuid!): Listener

  """
  fetch data from the table: "MediaType"
  """
  MediaType(
    """distinct select on columns"""
    distinct_on: [MediaType_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [MediaType_order_by!]

    """filter the rows returned"""
    where: MediaType_bool_exp
  ): [MediaType!]!

  """
  fetch aggregated fields from the table: "MediaType"
  """
  MediaType_aggregate(
    """distinct select on columns"""
    distinct_on: [MediaType_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [MediaType_order_by!]

    """filter the rows returned"""
    where: MediaType_bool_exp
  ): MediaType_aggregate!

  """fetch data from the table: "MediaType" using primary key columns"""
  MediaType_by_pk(MediaTypeId: Int!): MediaType

  """
  fetch data from the table: "Playlist"
  """
  Playlist(
    """distinct select on columns"""
    distinct_on: [Playlist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Playlist_order_by!]

    """filter the rows returned"""
    where: Playlist_bool_exp
  ): [Playlist!]!

  """
  fetch data from the table: "PlaylistTrack"
  """
  PlaylistTrack(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): [PlaylistTrack!]!

  """
  fetch aggregated fields from the table: "PlaylistTrack"
  """
  PlaylistTrack_aggregate(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): PlaylistTrack_aggregate!

  """fetch data from the table: "PlaylistTrack" using primary key columns"""
  PlaylistTrack_by_pk(PlaylistId: Int!, TrackId: Int!): PlaylistTrack

  """
  fetch aggregated fields from the table: "Playlist"
  """
  Playlist_aggregate(
    """distinct select on columns"""
    distinct_on: [Playlist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Playlist_order_by!]

    """filter the rows returned"""
    where: Playlist_bool_exp
  ): Playlist_aggregate!

  """fetch data from the table: "Playlist" using primary key columns"""
  Playlist_by_pk(PlaylistId: Int!): Playlist

  """
  fetch data from the table: "Track"
  """
  Track(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): [Track!]!

  """
  fetch data from the table: "TrackPlay"
  """
  TrackPlay(
    """distinct select on columns"""
    distinct_on: [TrackPlay_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [TrackPlay_order_by!]

    """filter the rows returned"""
    where: TrackPlay_bool_exp
  ): [TrackPlay!]!

  """
  fetch aggregated fields from the table: "TrackPlay"
  """
  TrackPlay_aggregate(
    """distinct select on columns"""
    distinct_on: [TrackPlay_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [TrackPlay_order_by!]

    """filter the rows returned"""
    where: TrackPlay_bool_exp
  ): TrackPlay_aggregate!

  """fetch data from the table: "TrackPlay" using primary key columns"""
  TrackPlay_by_pk(id: Int!): TrackPlay

  """
  fetch aggregated fields from the table: "Track"
  """
  Track_aggregate(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): Track_aggregate!

  """fetch data from the table: "Track" using primary key columns"""
  Track_by_pk(TrackId: Int!): Track
}

type subscription_root {
  """
  fetch data from the table: "Album"
  """
  Album(
    """distinct select on columns"""
    distinct_on: [Album_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Album_order_by!]

    """filter the rows returned"""
    where: Album_bool_exp
  ): [Album!]!

  """
  fetch aggregated fields from the table: "Album"
  """
  Album_aggregate(
    """distinct select on columns"""
    distinct_on: [Album_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Album_order_by!]

    """filter the rows returned"""
    where: Album_bool_exp
  ): Album_aggregate!

  """fetch data from the table: "Album" using primary key columns"""
  Album_by_pk(AlbumId: Int!): Album

  """
  fetch data from the table in a streaming manner: "Album"
  """
  Album_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Album_stream_cursor_input]!

    """filter the rows returned"""
    where: Album_bool_exp
  ): [Album!]!

  """
  fetch data from the table: "Artist"
  """
  Artist(
    """distinct select on columns"""
    distinct_on: [Artist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Artist_order_by!]

    """filter the rows returned"""
    where: Artist_bool_exp
  ): [Artist!]!

  """
  fetch aggregated fields from the table: "Artist"
  """
  Artist_aggregate(
    """distinct select on columns"""
    distinct_on: [Artist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Artist_order_by!]

    """filter the rows returned"""
    where: Artist_bool_exp
  ): Artist_aggregate!

  """fetch data from the table: "Artist" using primary key columns"""
  Artist_by_pk(ArtistId: Int!): Artist

  """
  fetch data from the table in a streaming manner: "Artist"
  """
  Artist_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Artist_stream_cursor_input]!

    """filter the rows returned"""
    where: Artist_bool_exp
  ): [Artist!]!

  """
  fetch data from the table: "Customer"
  """
  Customer(
    """distinct select on columns"""
    distinct_on: [Customer_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Customer_order_by!]

    """filter the rows returned"""
    where: Customer_bool_exp
  ): [Customer!]!

  """
  fetch aggregated fields from the table: "Customer"
  """
  Customer_aggregate(
    """distinct select on columns"""
    distinct_on: [Customer_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Customer_order_by!]

    """filter the rows returned"""
    where: Customer_bool_exp
  ): Customer_aggregate!

  """fetch data from the table: "Customer" using primary key columns"""
  Customer_by_pk(CustomerId: Int!): Customer

  """
  fetch data from the table in a streaming manner: "Customer"
  """
  Customer_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Customer_stream_cursor_input]!

    """filter the rows returned"""
    where: Customer_bool_exp
  ): [Customer!]!

  """
  fetch data from the table: "Employee"
  """
  Employee(
    """distinct select on columns"""
    distinct_on: [Employee_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Employee_order_by!]

    """filter the rows returned"""
    where: Employee_bool_exp
  ): [Employee!]!

  """
  fetch aggregated fields from the table: "Employee"
  """
  Employee_aggregate(
    """distinct select on columns"""
    distinct_on: [Employee_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Employee_order_by!]

    """filter the rows returned"""
    where: Employee_bool_exp
  ): Employee_aggregate!

  """fetch data from the table: "Employee" using primary key columns"""
  Employee_by_pk(EmployeeId: Int!): Employee

  """
  fetch data from the table in a streaming manner: "Employee"
  """
  Employee_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Employee_stream_cursor_input]!

    """filter the rows returned"""
    where: Employee_bool_exp
  ): [Employee!]!

  """
  fetch data from the table: "Genre"
  """
  Genre(
    """distinct select on columns"""
    distinct_on: [Genre_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Genre_order_by!]

    """filter the rows returned"""
    where: Genre_bool_exp
  ): [Genre!]!

  """
  fetch aggregated fields from the table: "Genre"
  """
  Genre_aggregate(
    """distinct select on columns"""
    distinct_on: [Genre_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Genre_order_by!]

    """filter the rows returned"""
    where: Genre_bool_exp
  ): Genre_aggregate!

  """fetch data from the table: "Genre" using primary key columns"""
  Genre_by_pk(GenreId: Int!): Genre

  """
  fetch data from the table in a streaming manner: "Genre"
  """
  Genre_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Genre_stream_cursor_input]!

    """filter the rows returned"""
    where: Genre_bool_exp
  ): [Genre!]!

  """
  fetch data from the table: "Invoice"
  """
  Invoice(
    """distinct select on columns"""
    distinct_on: [Invoice_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Invoice_order_by!]

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): [Invoice!]!

  """
  fetch data from the table: "InvoiceLine"
  """
  InvoiceLine(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): [InvoiceLine!]!

  """
  fetch aggregated fields from the table: "InvoiceLine"
  """
  InvoiceLine_aggregate(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): InvoiceLine_aggregate!

  """fetch data from the table: "InvoiceLine" using primary key columns"""
  InvoiceLine_by_pk(InvoiceLineId: Int!): InvoiceLine

  """
  fetch data from the table in a streaming manner: "InvoiceLine"
  """
  InvoiceLine_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [InvoiceLine_stream_cursor_input]!

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): [InvoiceLine!]!

  """
  fetch aggregated fields from the table: "Invoice"
  """
  Invoice_aggregate(
    """distinct select on columns"""
    distinct_on: [Invoice_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Invoice_order_by!]

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): Invoice_aggregate!

  """fetch data from the table: "Invoice" using primary key columns"""
  Invoice_by_pk(InvoiceId: Int!): Invoice

  """
  fetch data from the table in a streaming manner: "Invoice"
  """
  Invoice_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Invoice_stream_cursor_input]!

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): [Invoice!]!

  """
  fetch data from the table: "Listener"
  """
  Listener(
    """distinct select on columns"""
    distinct_on: [Listener_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Listener_order_by!]

    """filter the rows returned"""
    where: Listener_bool_exp
  ): [Listener!]!

  """
  fetch aggregated fields from the table: "Listener"
  """
  Listener_aggregate(
    """distinct select on columns"""
    distinct_on: [Listener_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Listener_order_by!]

    """filter the rows returned"""
    where: Listener_bool_exp
  ): Listener_aggregate!

  """fetch data from the table: "Listener" using primary key columns"""
  Listener_by_pk(id: uuid!): Listener

  """
  fetch data from the table in a streaming manner: "Listener"
  """
  Listener_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Listener_stream_cursor_input]!

    """filter the rows returned"""
    where: Listener_bool_exp
  ): [Listener!]!

  """
  fetch data from the table: "MediaType"
  """
  MediaType(
    """distinct select on columns"""
    distinct_on: [MediaType_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [MediaType_order_by!]

    """filter the rows returned"""
    where: MediaType_bool_exp
  ): [MediaType!]!

  """
  fetch aggregated fields from the table: "MediaType"
  """
  MediaType_aggregate(
    """distinct select on columns"""
    distinct_on: [MediaType_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [MediaType_order_by!]

    """filter the rows returned"""
    where: MediaType_bool_exp
  ): MediaType_aggregate!

  """fetch data from the table: "MediaType" using primary key columns"""
  MediaType_by_pk(MediaTypeId: Int!): MediaType

  """
  fetch data from the table in a streaming manner: "MediaType"
  """
  MediaType_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [MediaType_stream_cursor_input]!

    """filter the rows returned"""
    where: MediaType_bool_exp
  ): [MediaType!]!

  """
  fetch data from the table: "Playlist"
  """
  Playlist(
    """distinct select on columns"""
    distinct_on: [Playlist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Playlist_order_by!]

    """filter the rows returned"""
    where: Playlist_bool_exp
  ): [Playlist!]!

  """
  fetch data from the table: "PlaylistTrack"
  """
  PlaylistTrack(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): [PlaylistTrack!]!

  """
  fetch aggregated fields from the table: "PlaylistTrack"
  """
  PlaylistTrack_aggregate(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): PlaylistTrack_aggregate!

  """fetch data from the table: "PlaylistTrack" using primary key columns"""
  PlaylistTrack_by_pk(PlaylistId: Int!, TrackId: Int!): PlaylistTrack

  """
  fetch data from the table in a streaming manner: "PlaylistTrack"
  """
  PlaylistTrack_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [PlaylistTrack_stream_cursor_input]!

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): [PlaylistTrack!]!

  """
  fetch aggregated fields from the table: "Playlist"
  """
  Playlist_aggregate(
    """distinct select on columns"""
    distinct_on: [Playlist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Playlist_order_by!]

    """filter the rows returned"""
    where: Playlist_bool_exp
  ): Playlist_aggregate!

  """fetch data from the table: "Playlist" using primary key columns"""
  Playlist_by_pk(PlaylistId: Int!): Playlist

  """
  fetch data from the table in a streaming manner: "Playlist"
  """
  Playlist_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Playlist_stream_cursor_input]!

    """filter the rows returned"""
    where: Playlist_bool_exp
  ): [Playlist!]!

  """
  fetch data from the table: "Track"
  """
  Track(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): [Track!]!

  """
  fetch data from the table: "TrackPlay"
  """
  TrackPlay(
    """distinct select on columns"""
    distinct_on: [TrackPlay_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [TrackPlay_order_by!]

    """filter the rows returned"""
    where: TrackPlay_bool_exp
  ): [TrackPlay!]!

  """
  fetch aggregated fields from the table: "TrackPlay"
  """
  TrackPlay_aggregate(
    """distinct select on columns"""
    distinct_on: [TrackPlay_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [TrackPlay_order_by!]

    """filter the rows returned"""
    where: TrackPlay_bool_exp
  ): TrackPlay_aggregate!

  """fetch data from the table: "TrackPlay" using primary key columns"""
  TrackPlay_by_pk(id: Int!): TrackPlay

  """
  fetch data from the table in a streaming manner: "TrackPlay"
  """
  TrackPlay_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [TrackPlay_stream_cursor_input]!

    """filter the rows returned"""
    where: TrackPlay_bool_exp
  ): [TrackPlay!]!

  """
  fetch aggregated fields from the table: "Track"
  """
  Track_aggregate(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): Track_aggregate!

  """fetch data from the table: "Track" using primary key columns"""
  Track_by_pk(TrackId: Int!): Track

  """
  fetch data from the table in a streaming manner: "Track"
  """
  Track_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Track_stream_cursor_input]!

    """filter the rows returned"""
    where: Track_bool_exp
  ): [Track!]!
}

scalar timestamp

"""
Boolean expression to compare columns of type "timestamp". All fields are combined with logical 'AND'.
"""
input timestamp_comparison_exp {
  _eq: timestamp
  _gt: timestamp
  _gte: timestamp
  _in: [timestamp!]
  _is_null: Boolean
  _lt: timestamp
  _lte: timestamp
  _neq: timestamp
  _nin: [timestamp!]
}

scalar timestamptz

"""
Boolean expression to compare columns of type "timestamptz". All fields are combined with logical 'AND'.
"""
input timestamptz_comparison_exp {
  _eq: timestamptz
  _gt: timestamptz
  _gte: timestamptz
  _in: [timestamptz!]
  _is_null: Boolean
  _lt: timestamptz
  _lte: timestamptz
  _neq: timestamptz
  _nin: [timestamptz!]
}

scalar uuid

"""
Boolean expression to compare columns of type "uuid". All fields are combined with logical 'AND'.
"""
input uuid_comparison_exp {
  _eq: uuid
  _gt: uuid
  _gte: uuid
  _in: [uuid!]
  _is_null: Boolean
  _lt: uuid
  _lte: uuid
  _neq: uuid
  _nin: [uuid!]
}
`;

export const chinook3 = `
schema {
  query: query_root
  mutation: mutation_root
  subscription: subscription_root
}

"""whether this query should be cached (Hasura Cloud only)"""
directive @cached(
  """measured in seconds"""
  ttl: Int! = 60

  """refresh the cache entry"""
  refresh: Boolean! = false
) on QUERY

"""
columns and relationships of "Album"
"""
type Album {
  AlbumId: Int!

  """An object relationship"""
  Artist: Artist!
  ArtistId: Int!
  Title: String!

  """An array relationship"""
  Tracks(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): [Track!]!

  """An aggregate relationship"""
  Tracks_aggregate(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): Track_aggregate!
}

"""
aggregated selection of "Album"
"""
type Album_aggregate {
  aggregate: Album_aggregate_fields
  nodes: [Album!]!
}

input Album_aggregate_bool_exp {
  count: Album_aggregate_bool_exp_count
}

input Album_aggregate_bool_exp_count {
  arguments: [Album_select_column!]
  distinct: Boolean
  filter: Album_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "Album"
"""
type Album_aggregate_fields {
  avg: Album_avg_fields
  count(columns: [Album_select_column!], distinct: Boolean): Int!
  max: Album_max_fields
  min: Album_min_fields
  stddev: Album_stddev_fields
  stddev_pop: Album_stddev_pop_fields
  stddev_samp: Album_stddev_samp_fields
  sum: Album_sum_fields
  var_pop: Album_var_pop_fields
  var_samp: Album_var_samp_fields
  variance: Album_variance_fields
}

"""
order by aggregate values of table "Album"
"""
input Album_aggregate_order_by {
  avg: Album_avg_order_by
  count: order_by
  max: Album_max_order_by
  min: Album_min_order_by
  stddev: Album_stddev_order_by
  stddev_pop: Album_stddev_pop_order_by
  stddev_samp: Album_stddev_samp_order_by
  sum: Album_sum_order_by
  var_pop: Album_var_pop_order_by
  var_samp: Album_var_samp_order_by
  variance: Album_variance_order_by
}

"""
input type for inserting array relation for remote table "Album"
"""
input Album_arr_rel_insert_input {
  data: [Album_insert_input!]!

  """upsert condition"""
  on_conflict: Album_on_conflict
}

"""aggregate avg on columns"""
type Album_avg_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by avg() on columns of table "Album"
"""
input Album_avg_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""
Boolean expression to filter rows from the table "Album". All fields are combined with a logical 'AND'.
"""
input Album_bool_exp {
  AlbumId: Int_comparison_exp
  Artist: Artist_bool_exp
  ArtistId: Int_comparison_exp
  Title: String_comparison_exp
  Tracks: Track_bool_exp
  Tracks_aggregate: Track_aggregate_bool_exp
  _and: [Album_bool_exp!]
  _not: Album_bool_exp
  _or: [Album_bool_exp!]
}

"""
unique or primary key constraints on table "Album"
"""
enum Album_constraint {
  """
  unique or primary key constraint on columns "AlbumId"
  """
  PK_Album
}

"""
input type for incrementing numeric columns in table "Album"
"""
input Album_inc_input {
  AlbumId: Int
  ArtistId: Int
}

"""
input type for inserting data into table "Album"
"""
input Album_insert_input {
  AlbumId: Int
  Artist: Artist_obj_rel_insert_input
  ArtistId: Int
  Title: String
  Tracks: Track_arr_rel_insert_input
}

"""aggregate max on columns"""
type Album_max_fields {
  AlbumId: Int
  ArtistId: Int
  Title: String
}

"""
order by max() on columns of table "Album"
"""
input Album_max_order_by {
  AlbumId: order_by
  ArtistId: order_by
  Title: order_by
}

"""aggregate min on columns"""
type Album_min_fields {
  AlbumId: Int
  ArtistId: Int
  Title: String
}

"""
order by min() on columns of table "Album"
"""
input Album_min_order_by {
  AlbumId: order_by
  ArtistId: order_by
  Title: order_by
}

"""
response of any mutation on the table "Album"
"""
type Album_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Album!]!
}

"""
input type for inserting object relation for remote table "Album"
"""
input Album_obj_rel_insert_input {
  data: Album_insert_input!

  """upsert condition"""
  on_conflict: Album_on_conflict
}

"""
on_conflict condition type for table "Album"
"""
input Album_on_conflict {
  constraint: Album_constraint!
  update_columns: [Album_update_column!]! = []
  where: Album_bool_exp
}

"""Ordering options when selecting data from "Album"."""
input Album_order_by {
  AlbumId: order_by
  Artist: Artist_order_by
  ArtistId: order_by
  Title: order_by
  Tracks_aggregate: Track_aggregate_order_by
}

"""primary key columns input for table: Album"""
input Album_pk_columns_input {
  AlbumId: Int!
}

"""
select columns of table "Album"
"""
enum Album_select_column {
  """column name"""
  AlbumId

  """column name"""
  ArtistId

  """column name"""
  Title
}

"""
input type for updating data in table "Album"
"""
input Album_set_input {
  AlbumId: Int
  ArtistId: Int
  Title: String
}

"""aggregate stddev on columns"""
type Album_stddev_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by stddev() on columns of table "Album"
"""
input Album_stddev_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""aggregate stddev_pop on columns"""
type Album_stddev_pop_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by stddev_pop() on columns of table "Album"
"""
input Album_stddev_pop_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""aggregate stddev_samp on columns"""
type Album_stddev_samp_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by stddev_samp() on columns of table "Album"
"""
input Album_stddev_samp_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""
Streaming cursor of the table "Album"
"""
input Album_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Album_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Album_stream_cursor_value_input {
  AlbumId: Int
  ArtistId: Int
  Title: String
}

"""aggregate sum on columns"""
type Album_sum_fields {
  AlbumId: Int
  ArtistId: Int
}

"""
order by sum() on columns of table "Album"
"""
input Album_sum_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""
update columns of table "Album"
"""
enum Album_update_column {
  """column name"""
  AlbumId

  """column name"""
  ArtistId

  """column name"""
  Title
}

input Album_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Album_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Album_set_input

  """filter the rows which have to be updated"""
  where: Album_bool_exp!
}

"""aggregate var_pop on columns"""
type Album_var_pop_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by var_pop() on columns of table "Album"
"""
input Album_var_pop_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""aggregate var_samp on columns"""
type Album_var_samp_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by var_samp() on columns of table "Album"
"""
input Album_var_samp_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""aggregate variance on columns"""
type Album_variance_fields {
  AlbumId: Float
  ArtistId: Float
}

"""
order by variance() on columns of table "Album"
"""
input Album_variance_order_by {
  AlbumId: order_by
  ArtistId: order_by
}

"""
columns and relationships of "Artist"
"""
type Artist {
  """An array relationship"""
  Albums(
    """distinct select on columns"""
    distinct_on: [Album_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Album_order_by!]

    """filter the rows returned"""
    where: Album_bool_exp
  ): [Album!]!

  """An aggregate relationship"""
  Albums_aggregate(
    """distinct select on columns"""
    distinct_on: [Album_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Album_order_by!]

    """filter the rows returned"""
    where: Album_bool_exp
  ): Album_aggregate!
  ArtistId: Int!
  Name: String
}

"""
aggregated selection of "Artist"
"""
type Artist_aggregate {
  aggregate: Artist_aggregate_fields
  nodes: [Artist!]!
}

"""
aggregate fields of "Artist"
"""
type Artist_aggregate_fields {
  avg: Artist_avg_fields
  count(columns: [Artist_select_column!], distinct: Boolean): Int!
  max: Artist_max_fields
  min: Artist_min_fields
  stddev: Artist_stddev_fields
  stddev_pop: Artist_stddev_pop_fields
  stddev_samp: Artist_stddev_samp_fields
  sum: Artist_sum_fields
  var_pop: Artist_var_pop_fields
  var_samp: Artist_var_samp_fields
  variance: Artist_variance_fields
}

"""aggregate avg on columns"""
type Artist_avg_fields {
  ArtistId: Float
}

"""
Boolean expression to filter rows from the table "Artist". All fields are combined with a logical 'AND'.
"""
input Artist_bool_exp {
  Albums: Album_bool_exp
  Albums_aggregate: Album_aggregate_bool_exp
  ArtistId: Int_comparison_exp
  Name: String_comparison_exp
  _and: [Artist_bool_exp!]
  _not: Artist_bool_exp
  _or: [Artist_bool_exp!]
}

"""
unique or primary key constraints on table "Artist"
"""
enum Artist_constraint {
  """
  unique or primary key constraint on columns "ArtistId"
  """
  PK_Artist
}

"""
input type for incrementing numeric columns in table "Artist"
"""
input Artist_inc_input {
  ArtistId: Int
}

"""
input type for inserting data into table "Artist"
"""
input Artist_insert_input {
  Albums: Album_arr_rel_insert_input
  ArtistId: Int
  Name: String
}

"""aggregate max on columns"""
type Artist_max_fields {
  ArtistId: Int
  Name: String
}

"""aggregate min on columns"""
type Artist_min_fields {
  ArtistId: Int
  Name: String
}

"""
response of any mutation on the table "Artist"
"""
type Artist_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Artist!]!
}

"""
input type for inserting object relation for remote table "Artist"
"""
input Artist_obj_rel_insert_input {
  data: Artist_insert_input!

  """upsert condition"""
  on_conflict: Artist_on_conflict
}

"""
on_conflict condition type for table "Artist"
"""
input Artist_on_conflict {
  constraint: Artist_constraint!
  update_columns: [Artist_update_column!]! = []
  where: Artist_bool_exp
}

"""Ordering options when selecting data from "Artist"."""
input Artist_order_by {
  Albums_aggregate: Album_aggregate_order_by
  ArtistId: order_by
  Name: order_by
}

"""primary key columns input for table: Artist"""
input Artist_pk_columns_input {
  ArtistId: Int!
}

"""
select columns of table "Artist"
"""
enum Artist_select_column {
  """column name"""
  ArtistId

  """column name"""
  Name
}

"""
input type for updating data in table "Artist"
"""
input Artist_set_input {
  ArtistId: Int
  Name: String
}

"""aggregate stddev on columns"""
type Artist_stddev_fields {
  ArtistId: Float
}

"""aggregate stddev_pop on columns"""
type Artist_stddev_pop_fields {
  ArtistId: Float
}

"""aggregate stddev_samp on columns"""
type Artist_stddev_samp_fields {
  ArtistId: Float
}

"""
Streaming cursor of the table "Artist"
"""
input Artist_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Artist_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Artist_stream_cursor_value_input {
  ArtistId: Int
  Name: String
}

"""aggregate sum on columns"""
type Artist_sum_fields {
  ArtistId: Int
}

"""
update columns of table "Artist"
"""
enum Artist_update_column {
  """column name"""
  ArtistId

  """column name"""
  Name
}

input Artist_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Artist_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Artist_set_input

  """filter the rows which have to be updated"""
  where: Artist_bool_exp!
}

"""aggregate var_pop on columns"""
type Artist_var_pop_fields {
  ArtistId: Float
}

"""aggregate var_samp on columns"""
type Artist_var_samp_fields {
  ArtistId: Float
}

"""aggregate variance on columns"""
type Artist_variance_fields {
  ArtistId: Float
}

"""
columns and relationships of "Customer"
"""
type Customer {
  Address: String
  City: String
  Company: String
  Country: String
  CustomerId: Int!
  Email: String!

  """An object relationship"""
  Employee: Employee
  Fax: String
  FirstName: String!

  """An array relationship"""
  Invoices(
    """distinct select on columns"""
    distinct_on: [Invoice_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Invoice_order_by!]

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): [Invoice!]!

  """An aggregate relationship"""
  Invoices_aggregate(
    """distinct select on columns"""
    distinct_on: [Invoice_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Invoice_order_by!]

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): Invoice_aggregate!
  LastName: String!
  Phone: String
  PostalCode: String
  State: String
  SupportRepId: Int
}

"""
aggregated selection of "Customer"
"""
type Customer_aggregate {
  aggregate: Customer_aggregate_fields
  nodes: [Customer!]!
}

input Customer_aggregate_bool_exp {
  count: Customer_aggregate_bool_exp_count
}

input Customer_aggregate_bool_exp_count {
  arguments: [Customer_select_column!]
  distinct: Boolean
  filter: Customer_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "Customer"
"""
type Customer_aggregate_fields {
  avg: Customer_avg_fields
  count(columns: [Customer_select_column!], distinct: Boolean): Int!
  max: Customer_max_fields
  min: Customer_min_fields
  stddev: Customer_stddev_fields
  stddev_pop: Customer_stddev_pop_fields
  stddev_samp: Customer_stddev_samp_fields
  sum: Customer_sum_fields
  var_pop: Customer_var_pop_fields
  var_samp: Customer_var_samp_fields
  variance: Customer_variance_fields
}

"""
order by aggregate values of table "Customer"
"""
input Customer_aggregate_order_by {
  avg: Customer_avg_order_by
  count: order_by
  max: Customer_max_order_by
  min: Customer_min_order_by
  stddev: Customer_stddev_order_by
  stddev_pop: Customer_stddev_pop_order_by
  stddev_samp: Customer_stddev_samp_order_by
  sum: Customer_sum_order_by
  var_pop: Customer_var_pop_order_by
  var_samp: Customer_var_samp_order_by
  variance: Customer_variance_order_by
}

"""
input type for inserting array relation for remote table "Customer"
"""
input Customer_arr_rel_insert_input {
  data: [Customer_insert_input!]!

  """upsert condition"""
  on_conflict: Customer_on_conflict
}

"""aggregate avg on columns"""
type Customer_avg_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by avg() on columns of table "Customer"
"""
input Customer_avg_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""
Boolean expression to filter rows from the table "Customer". All fields are combined with a logical 'AND'.
"""
input Customer_bool_exp {
  Address: String_comparison_exp
  City: String_comparison_exp
  Company: String_comparison_exp
  Country: String_comparison_exp
  CustomerId: Int_comparison_exp
  Email: String_comparison_exp
  Employee: Employee_bool_exp
  Fax: String_comparison_exp
  FirstName: String_comparison_exp
  Invoices: Invoice_bool_exp
  Invoices_aggregate: Invoice_aggregate_bool_exp
  LastName: String_comparison_exp
  Phone: String_comparison_exp
  PostalCode: String_comparison_exp
  State: String_comparison_exp
  SupportRepId: Int_comparison_exp
  _and: [Customer_bool_exp!]
  _not: Customer_bool_exp
  _or: [Customer_bool_exp!]
}

"""
unique or primary key constraints on table "Customer"
"""
enum Customer_constraint {
  """
  unique or primary key constraint on columns "CustomerId"
  """
  PK_Customer
}

"""
input type for incrementing numeric columns in table "Customer"
"""
input Customer_inc_input {
  CustomerId: Int
  SupportRepId: Int
}

"""
input type for inserting data into table "Customer"
"""
input Customer_insert_input {
  Address: String
  City: String
  Company: String
  Country: String
  CustomerId: Int
  Email: String
  Employee: Employee_obj_rel_insert_input
  Fax: String
  FirstName: String
  Invoices: Invoice_arr_rel_insert_input
  LastName: String
  Phone: String
  PostalCode: String
  State: String
  SupportRepId: Int
}

"""aggregate max on columns"""
type Customer_max_fields {
  Address: String
  City: String
  Company: String
  Country: String
  CustomerId: Int
  Email: String
  Fax: String
  FirstName: String
  LastName: String
  Phone: String
  PostalCode: String
  State: String
  SupportRepId: Int
}

"""
order by max() on columns of table "Customer"
"""
input Customer_max_order_by {
  Address: order_by
  City: order_by
  Company: order_by
  Country: order_by
  CustomerId: order_by
  Email: order_by
  Fax: order_by
  FirstName: order_by
  LastName: order_by
  Phone: order_by
  PostalCode: order_by
  State: order_by
  SupportRepId: order_by
}

"""aggregate min on columns"""
type Customer_min_fields {
  Address: String
  City: String
  Company: String
  Country: String
  CustomerId: Int
  Email: String
  Fax: String
  FirstName: String
  LastName: String
  Phone: String
  PostalCode: String
  State: String
  SupportRepId: Int
}

"""
order by min() on columns of table "Customer"
"""
input Customer_min_order_by {
  Address: order_by
  City: order_by
  Company: order_by
  Country: order_by
  CustomerId: order_by
  Email: order_by
  Fax: order_by
  FirstName: order_by
  LastName: order_by
  Phone: order_by
  PostalCode: order_by
  State: order_by
  SupportRepId: order_by
}

"""
response of any mutation on the table "Customer"
"""
type Customer_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Customer!]!
}

"""
input type for inserting object relation for remote table "Customer"
"""
input Customer_obj_rel_insert_input {
  data: Customer_insert_input!

  """upsert condition"""
  on_conflict: Customer_on_conflict
}

"""
on_conflict condition type for table "Customer"
"""
input Customer_on_conflict {
  constraint: Customer_constraint!
  update_columns: [Customer_update_column!]! = []
  where: Customer_bool_exp
}

"""Ordering options when selecting data from "Customer"."""
input Customer_order_by {
  Address: order_by
  City: order_by
  Company: order_by
  Country: order_by
  CustomerId: order_by
  Email: order_by
  Employee: Employee_order_by
  Fax: order_by
  FirstName: order_by
  Invoices_aggregate: Invoice_aggregate_order_by
  LastName: order_by
  Phone: order_by
  PostalCode: order_by
  State: order_by
  SupportRepId: order_by
}

"""primary key columns input for table: Customer"""
input Customer_pk_columns_input {
  CustomerId: Int!
}

"""
select columns of table "Customer"
"""
enum Customer_select_column {
  """column name"""
  Address

  """column name"""
  City

  """column name"""
  Company

  """column name"""
  Country

  """column name"""
  CustomerId

  """column name"""
  Email

  """column name"""
  Fax

  """column name"""
  FirstName

  """column name"""
  LastName

  """column name"""
  Phone

  """column name"""
  PostalCode

  """column name"""
  State

  """column name"""
  SupportRepId
}

"""
input type for updating data in table "Customer"
"""
input Customer_set_input {
  Address: String
  City: String
  Company: String
  Country: String
  CustomerId: Int
  Email: String
  Fax: String
  FirstName: String
  LastName: String
  Phone: String
  PostalCode: String
  State: String
  SupportRepId: Int
}

"""aggregate stddev on columns"""
type Customer_stddev_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by stddev() on columns of table "Customer"
"""
input Customer_stddev_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""aggregate stddev_pop on columns"""
type Customer_stddev_pop_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by stddev_pop() on columns of table "Customer"
"""
input Customer_stddev_pop_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""aggregate stddev_samp on columns"""
type Customer_stddev_samp_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by stddev_samp() on columns of table "Customer"
"""
input Customer_stddev_samp_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""
Streaming cursor of the table "Customer"
"""
input Customer_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Customer_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Customer_stream_cursor_value_input {
  Address: String
  City: String
  Company: String
  Country: String
  CustomerId: Int
  Email: String
  Fax: String
  FirstName: String
  LastName: String
  Phone: String
  PostalCode: String
  State: String
  SupportRepId: Int
}

"""aggregate sum on columns"""
type Customer_sum_fields {
  CustomerId: Int
  SupportRepId: Int
}

"""
order by sum() on columns of table "Customer"
"""
input Customer_sum_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""
update columns of table "Customer"
"""
enum Customer_update_column {
  """column name"""
  Address

  """column name"""
  City

  """column name"""
  Company

  """column name"""
  Country

  """column name"""
  CustomerId

  """column name"""
  Email

  """column name"""
  Fax

  """column name"""
  FirstName

  """column name"""
  LastName

  """column name"""
  Phone

  """column name"""
  PostalCode

  """column name"""
  State

  """column name"""
  SupportRepId
}

input Customer_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Customer_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Customer_set_input

  """filter the rows which have to be updated"""
  where: Customer_bool_exp!
}

"""aggregate var_pop on columns"""
type Customer_var_pop_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by var_pop() on columns of table "Customer"
"""
input Customer_var_pop_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""aggregate var_samp on columns"""
type Customer_var_samp_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by var_samp() on columns of table "Customer"
"""
input Customer_var_samp_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""aggregate variance on columns"""
type Customer_variance_fields {
  CustomerId: Float
  SupportRepId: Float
}

"""
order by variance() on columns of table "Customer"
"""
input Customer_variance_order_by {
  CustomerId: order_by
  SupportRepId: order_by
}

"""
columns and relationships of "Employee"
"""
type Employee {
  Address: String
  BirthDate: timestamp
  City: String
  Country: String

  """An array relationship"""
  Customers(
    """distinct select on columns"""
    distinct_on: [Customer_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Customer_order_by!]

    """filter the rows returned"""
    where: Customer_bool_exp
  ): [Customer!]!

  """An aggregate relationship"""
  Customers_aggregate(
    """distinct select on columns"""
    distinct_on: [Customer_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Customer_order_by!]

    """filter the rows returned"""
    where: Customer_bool_exp
  ): Customer_aggregate!
  Email: String

  """An object relationship"""
  Employee: Employee
  EmployeeId: Int!

  """An array relationship"""
  Employees(
    """distinct select on columns"""
    distinct_on: [Employee_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Employee_order_by!]

    """filter the rows returned"""
    where: Employee_bool_exp
  ): [Employee!]!

  """An aggregate relationship"""
  Employees_aggregate(
    """distinct select on columns"""
    distinct_on: [Employee_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Employee_order_by!]

    """filter the rows returned"""
    where: Employee_bool_exp
  ): Employee_aggregate!
  Fax: String
  FirstName: String!
  HireDate: timestamp
  LastName: String!
  Phone: String
  PostalCode: String
  ReportsTo: Int
  State: String
  Title: String
}

"""
aggregated selection of "Employee"
"""
type Employee_aggregate {
  aggregate: Employee_aggregate_fields
  nodes: [Employee!]!
}

input Employee_aggregate_bool_exp {
  count: Employee_aggregate_bool_exp_count
}

input Employee_aggregate_bool_exp_count {
  arguments: [Employee_select_column!]
  distinct: Boolean
  filter: Employee_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "Employee"
"""
type Employee_aggregate_fields {
  avg: Employee_avg_fields
  count(columns: [Employee_select_column!], distinct: Boolean): Int!
  max: Employee_max_fields
  min: Employee_min_fields
  stddev: Employee_stddev_fields
  stddev_pop: Employee_stddev_pop_fields
  stddev_samp: Employee_stddev_samp_fields
  sum: Employee_sum_fields
  var_pop: Employee_var_pop_fields
  var_samp: Employee_var_samp_fields
  variance: Employee_variance_fields
}

"""
order by aggregate values of table "Employee"
"""
input Employee_aggregate_order_by {
  avg: Employee_avg_order_by
  count: order_by
  max: Employee_max_order_by
  min: Employee_min_order_by
  stddev: Employee_stddev_order_by
  stddev_pop: Employee_stddev_pop_order_by
  stddev_samp: Employee_stddev_samp_order_by
  sum: Employee_sum_order_by
  var_pop: Employee_var_pop_order_by
  var_samp: Employee_var_samp_order_by
  variance: Employee_variance_order_by
}

"""
input type for inserting array relation for remote table "Employee"
"""
input Employee_arr_rel_insert_input {
  data: [Employee_insert_input!]!

  """upsert condition"""
  on_conflict: Employee_on_conflict
}

"""aggregate avg on columns"""
type Employee_avg_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by avg() on columns of table "Employee"
"""
input Employee_avg_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""
Boolean expression to filter rows from the table "Employee". All fields are combined with a logical 'AND'.
"""
input Employee_bool_exp {
  Address: String_comparison_exp
  BirthDate: timestamp_comparison_exp
  City: String_comparison_exp
  Country: String_comparison_exp
  Customers: Customer_bool_exp
  Customers_aggregate: Customer_aggregate_bool_exp
  Email: String_comparison_exp
  Employee: Employee_bool_exp
  EmployeeId: Int_comparison_exp
  Employees: Employee_bool_exp
  Employees_aggregate: Employee_aggregate_bool_exp
  Fax: String_comparison_exp
  FirstName: String_comparison_exp
  HireDate: timestamp_comparison_exp
  LastName: String_comparison_exp
  Phone: String_comparison_exp
  PostalCode: String_comparison_exp
  ReportsTo: Int_comparison_exp
  State: String_comparison_exp
  Title: String_comparison_exp
  _and: [Employee_bool_exp!]
  _not: Employee_bool_exp
  _or: [Employee_bool_exp!]
}

"""
unique or primary key constraints on table "Employee"
"""
enum Employee_constraint {
  """
  unique or primary key constraint on columns "EmployeeId"
  """
  PK_Employee
}

"""
input type for incrementing numeric columns in table "Employee"
"""
input Employee_inc_input {
  EmployeeId: Int
  ReportsTo: Int
}

"""
input type for inserting data into table "Employee"
"""
input Employee_insert_input {
  Address: String
  BirthDate: timestamp
  City: String
  Country: String
  Customers: Customer_arr_rel_insert_input
  Email: String
  Employee: Employee_obj_rel_insert_input
  EmployeeId: Int
  Employees: Employee_arr_rel_insert_input
  Fax: String
  FirstName: String
  HireDate: timestamp
  LastName: String
  Phone: String
  PostalCode: String
  ReportsTo: Int
  State: String
  Title: String
}

"""aggregate max on columns"""
type Employee_max_fields {
  Address: String
  BirthDate: timestamp
  City: String
  Country: String
  Email: String
  EmployeeId: Int
  Fax: String
  FirstName: String
  HireDate: timestamp
  LastName: String
  Phone: String
  PostalCode: String
  ReportsTo: Int
  State: String
  Title: String
}

"""
order by max() on columns of table "Employee"
"""
input Employee_max_order_by {
  Address: order_by
  BirthDate: order_by
  City: order_by
  Country: order_by
  Email: order_by
  EmployeeId: order_by
  Fax: order_by
  FirstName: order_by
  HireDate: order_by
  LastName: order_by
  Phone: order_by
  PostalCode: order_by
  ReportsTo: order_by
  State: order_by
  Title: order_by
}

"""aggregate min on columns"""
type Employee_min_fields {
  Address: String
  BirthDate: timestamp
  City: String
  Country: String
  Email: String
  EmployeeId: Int
  Fax: String
  FirstName: String
  HireDate: timestamp
  LastName: String
  Phone: String
  PostalCode: String
  ReportsTo: Int
  State: String
  Title: String
}

"""
order by min() on columns of table "Employee"
"""
input Employee_min_order_by {
  Address: order_by
  BirthDate: order_by
  City: order_by
  Country: order_by
  Email: order_by
  EmployeeId: order_by
  Fax: order_by
  FirstName: order_by
  HireDate: order_by
  LastName: order_by
  Phone: order_by
  PostalCode: order_by
  ReportsTo: order_by
  State: order_by
  Title: order_by
}

"""
response of any mutation on the table "Employee"
"""
type Employee_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Employee!]!
}

"""
input type for inserting object relation for remote table "Employee"
"""
input Employee_obj_rel_insert_input {
  data: Employee_insert_input!

  """upsert condition"""
  on_conflict: Employee_on_conflict
}

"""
on_conflict condition type for table "Employee"
"""
input Employee_on_conflict {
  constraint: Employee_constraint!
  update_columns: [Employee_update_column!]! = []
  where: Employee_bool_exp
}

"""Ordering options when selecting data from "Employee"."""
input Employee_order_by {
  Address: order_by
  BirthDate: order_by
  City: order_by
  Country: order_by
  Customers_aggregate: Customer_aggregate_order_by
  Email: order_by
  Employee: Employee_order_by
  EmployeeId: order_by
  Employees_aggregate: Employee_aggregate_order_by
  Fax: order_by
  FirstName: order_by
  HireDate: order_by
  LastName: order_by
  Phone: order_by
  PostalCode: order_by
  ReportsTo: order_by
  State: order_by
  Title: order_by
}

"""primary key columns input for table: Employee"""
input Employee_pk_columns_input {
  EmployeeId: Int!
}

"""
select columns of table "Employee"
"""
enum Employee_select_column {
  """column name"""
  Address

  """column name"""
  BirthDate

  """column name"""
  City

  """column name"""
  Country

  """column name"""
  Email

  """column name"""
  EmployeeId

  """column name"""
  Fax

  """column name"""
  FirstName

  """column name"""
  HireDate

  """column name"""
  LastName

  """column name"""
  Phone

  """column name"""
  PostalCode

  """column name"""
  ReportsTo

  """column name"""
  State

  """column name"""
  Title
}

"""
input type for updating data in table "Employee"
"""
input Employee_set_input {
  Address: String
  BirthDate: timestamp
  City: String
  Country: String
  Email: String
  EmployeeId: Int
  Fax: String
  FirstName: String
  HireDate: timestamp
  LastName: String
  Phone: String
  PostalCode: String
  ReportsTo: Int
  State: String
  Title: String
}

"""aggregate stddev on columns"""
type Employee_stddev_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by stddev() on columns of table "Employee"
"""
input Employee_stddev_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""aggregate stddev_pop on columns"""
type Employee_stddev_pop_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by stddev_pop() on columns of table "Employee"
"""
input Employee_stddev_pop_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""aggregate stddev_samp on columns"""
type Employee_stddev_samp_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by stddev_samp() on columns of table "Employee"
"""
input Employee_stddev_samp_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""
Streaming cursor of the table "Employee"
"""
input Employee_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Employee_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Employee_stream_cursor_value_input {
  Address: String
  BirthDate: timestamp
  City: String
  Country: String
  Email: String
  EmployeeId: Int
  Fax: String
  FirstName: String
  HireDate: timestamp
  LastName: String
  Phone: String
  PostalCode: String
  ReportsTo: Int
  State: String
  Title: String
}

"""aggregate sum on columns"""
type Employee_sum_fields {
  EmployeeId: Int
  ReportsTo: Int
}

"""
order by sum() on columns of table "Employee"
"""
input Employee_sum_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""
update columns of table "Employee"
"""
enum Employee_update_column {
  """column name"""
  Address

  """column name"""
  BirthDate

  """column name"""
  City

  """column name"""
  Country

  """column name"""
  Email

  """column name"""
  EmployeeId

  """column name"""
  Fax

  """column name"""
  FirstName

  """column name"""
  HireDate

  """column name"""
  LastName

  """column name"""
  Phone

  """column name"""
  PostalCode

  """column name"""
  ReportsTo

  """column name"""
  State

  """column name"""
  Title
}

input Employee_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Employee_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Employee_set_input

  """filter the rows which have to be updated"""
  where: Employee_bool_exp!
}

"""aggregate var_pop on columns"""
type Employee_var_pop_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by var_pop() on columns of table "Employee"
"""
input Employee_var_pop_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""aggregate var_samp on columns"""
type Employee_var_samp_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by var_samp() on columns of table "Employee"
"""
input Employee_var_samp_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""aggregate variance on columns"""
type Employee_variance_fields {
  EmployeeId: Float
  ReportsTo: Float
}

"""
order by variance() on columns of table "Employee"
"""
input Employee_variance_order_by {
  EmployeeId: order_by
  ReportsTo: order_by
}

"""
columns and relationships of "Genre"
"""
type Genre {
  GenreId: Int!
  Name: String

  """An array relationship"""
  Tracks(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): [Track!]!

  """An aggregate relationship"""
  Tracks_aggregate(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): Track_aggregate!
}

"""
aggregated selection of "Genre"
"""
type Genre_aggregate {
  aggregate: Genre_aggregate_fields
  nodes: [Genre!]!
}

"""
aggregate fields of "Genre"
"""
type Genre_aggregate_fields {
  avg: Genre_avg_fields
  count(columns: [Genre_select_column!], distinct: Boolean): Int!
  max: Genre_max_fields
  min: Genre_min_fields
  stddev: Genre_stddev_fields
  stddev_pop: Genre_stddev_pop_fields
  stddev_samp: Genre_stddev_samp_fields
  sum: Genre_sum_fields
  var_pop: Genre_var_pop_fields
  var_samp: Genre_var_samp_fields
  variance: Genre_variance_fields
}

"""aggregate avg on columns"""
type Genre_avg_fields {
  GenreId: Float
}

"""
Boolean expression to filter rows from the table "Genre". All fields are combined with a logical 'AND'.
"""
input Genre_bool_exp {
  GenreId: Int_comparison_exp
  Name: String_comparison_exp
  Tracks: Track_bool_exp
  Tracks_aggregate: Track_aggregate_bool_exp
  _and: [Genre_bool_exp!]
  _not: Genre_bool_exp
  _or: [Genre_bool_exp!]
}

"""
unique or primary key constraints on table "Genre"
"""
enum Genre_constraint {
  """
  unique or primary key constraint on columns "GenreId"
  """
  PK_Genre
}

"""
input type for incrementing numeric columns in table "Genre"
"""
input Genre_inc_input {
  GenreId: Int
}

"""
input type for inserting data into table "Genre"
"""
input Genre_insert_input {
  GenreId: Int
  Name: String
  Tracks: Track_arr_rel_insert_input
}

"""aggregate max on columns"""
type Genre_max_fields {
  GenreId: Int
  Name: String
}

"""aggregate min on columns"""
type Genre_min_fields {
  GenreId: Int
  Name: String
}

"""
response of any mutation on the table "Genre"
"""
type Genre_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Genre!]!
}

"""
input type for inserting object relation for remote table "Genre"
"""
input Genre_obj_rel_insert_input {
  data: Genre_insert_input!

  """upsert condition"""
  on_conflict: Genre_on_conflict
}

"""
on_conflict condition type for table "Genre"
"""
input Genre_on_conflict {
  constraint: Genre_constraint!
  update_columns: [Genre_update_column!]! = []
  where: Genre_bool_exp
}

"""Ordering options when selecting data from "Genre"."""
input Genre_order_by {
  GenreId: order_by
  Name: order_by
  Tracks_aggregate: Track_aggregate_order_by
}

"""primary key columns input for table: Genre"""
input Genre_pk_columns_input {
  GenreId: Int!
}

"""
select columns of table "Genre"
"""
enum Genre_select_column {
  """column name"""
  GenreId

  """column name"""
  Name
}

"""
input type for updating data in table "Genre"
"""
input Genre_set_input {
  GenreId: Int
  Name: String
}

"""aggregate stddev on columns"""
type Genre_stddev_fields {
  GenreId: Float
}

"""aggregate stddev_pop on columns"""
type Genre_stddev_pop_fields {
  GenreId: Float
}

"""aggregate stddev_samp on columns"""
type Genre_stddev_samp_fields {
  GenreId: Float
}

"""
Streaming cursor of the table "Genre"
"""
input Genre_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Genre_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Genre_stream_cursor_value_input {
  GenreId: Int
  Name: String
}

"""aggregate sum on columns"""
type Genre_sum_fields {
  GenreId: Int
}

"""
update columns of table "Genre"
"""
enum Genre_update_column {
  """column name"""
  GenreId

  """column name"""
  Name
}

input Genre_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Genre_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Genre_set_input

  """filter the rows which have to be updated"""
  where: Genre_bool_exp!
}

"""aggregate var_pop on columns"""
type Genre_var_pop_fields {
  GenreId: Float
}

"""aggregate var_samp on columns"""
type Genre_var_samp_fields {
  GenreId: Float
}

"""aggregate variance on columns"""
type Genre_variance_fields {
  GenreId: Float
}

"""
Boolean expression to compare columns of type "Int". All fields are combined with logical 'AND'.
"""
input Int_comparison_exp {
  _eq: Int
  _gt: Int
  _gte: Int
  _in: [Int!]
  _is_null: Boolean
  _lt: Int
  _lte: Int
  _neq: Int
  _nin: [Int!]
}

"""
columns and relationships of "Invoice"
"""
type Invoice {
  BillingAddress: String
  BillingCity: String
  BillingCountry: String
  BillingPostalCode: String
  BillingState: String

  """An object relationship"""
  Customer: Customer!
  CustomerId: Int!
  InvoiceDate: timestamp!
  InvoiceId: Int!

  """An array relationship"""
  InvoiceLines(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): [InvoiceLine!]!

  """An aggregate relationship"""
  InvoiceLines_aggregate(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): InvoiceLine_aggregate!
  Total: numeric!
}

"""
columns and relationships of "InvoiceLine"
"""
type InvoiceLine {
  """An object relationship"""
  Invoice: Invoice!
  InvoiceId: Int!
  InvoiceLineId: Int!
  Quantity: Int!

  """An object relationship"""
  Track: Track!
  TrackId: Int!
  UnitPrice: numeric!
}

"""
aggregated selection of "InvoiceLine"
"""
type InvoiceLine_aggregate {
  aggregate: InvoiceLine_aggregate_fields
  nodes: [InvoiceLine!]!
}

input InvoiceLine_aggregate_bool_exp {
  count: InvoiceLine_aggregate_bool_exp_count
}

input InvoiceLine_aggregate_bool_exp_count {
  arguments: [InvoiceLine_select_column!]
  distinct: Boolean
  filter: InvoiceLine_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "InvoiceLine"
"""
type InvoiceLine_aggregate_fields {
  avg: InvoiceLine_avg_fields
  count(columns: [InvoiceLine_select_column!], distinct: Boolean): Int!
  max: InvoiceLine_max_fields
  min: InvoiceLine_min_fields
  stddev: InvoiceLine_stddev_fields
  stddev_pop: InvoiceLine_stddev_pop_fields
  stddev_samp: InvoiceLine_stddev_samp_fields
  sum: InvoiceLine_sum_fields
  var_pop: InvoiceLine_var_pop_fields
  var_samp: InvoiceLine_var_samp_fields
  variance: InvoiceLine_variance_fields
}

"""
order by aggregate values of table "InvoiceLine"
"""
input InvoiceLine_aggregate_order_by {
  avg: InvoiceLine_avg_order_by
  count: order_by
  max: InvoiceLine_max_order_by
  min: InvoiceLine_min_order_by
  stddev: InvoiceLine_stddev_order_by
  stddev_pop: InvoiceLine_stddev_pop_order_by
  stddev_samp: InvoiceLine_stddev_samp_order_by
  sum: InvoiceLine_sum_order_by
  var_pop: InvoiceLine_var_pop_order_by
  var_samp: InvoiceLine_var_samp_order_by
  variance: InvoiceLine_variance_order_by
}

"""
input type for inserting array relation for remote table "InvoiceLine"
"""
input InvoiceLine_arr_rel_insert_input {
  data: [InvoiceLine_insert_input!]!

  """upsert condition"""
  on_conflict: InvoiceLine_on_conflict
}

"""aggregate avg on columns"""
type InvoiceLine_avg_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by avg() on columns of table "InvoiceLine"
"""
input InvoiceLine_avg_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
Boolean expression to filter rows from the table "InvoiceLine". All fields are combined with a logical 'AND'.
"""
input InvoiceLine_bool_exp {
  Invoice: Invoice_bool_exp
  InvoiceId: Int_comparison_exp
  InvoiceLineId: Int_comparison_exp
  Quantity: Int_comparison_exp
  Track: Track_bool_exp
  TrackId: Int_comparison_exp
  UnitPrice: numeric_comparison_exp
  _and: [InvoiceLine_bool_exp!]
  _not: InvoiceLine_bool_exp
  _or: [InvoiceLine_bool_exp!]
}

"""
unique or primary key constraints on table "InvoiceLine"
"""
enum InvoiceLine_constraint {
  """
  unique or primary key constraint on columns "InvoiceLineId"
  """
  PK_InvoiceLine
}

"""
input type for incrementing numeric columns in table "InvoiceLine"
"""
input InvoiceLine_inc_input {
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  TrackId: Int
  UnitPrice: numeric
}

"""
input type for inserting data into table "InvoiceLine"
"""
input InvoiceLine_insert_input {
  Invoice: Invoice_obj_rel_insert_input
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  Track: Track_obj_rel_insert_input
  TrackId: Int
  UnitPrice: numeric
}

"""aggregate max on columns"""
type InvoiceLine_max_fields {
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  TrackId: Int
  UnitPrice: numeric
}

"""
order by max() on columns of table "InvoiceLine"
"""
input InvoiceLine_max_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate min on columns"""
type InvoiceLine_min_fields {
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  TrackId: Int
  UnitPrice: numeric
}

"""
order by min() on columns of table "InvoiceLine"
"""
input InvoiceLine_min_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
response of any mutation on the table "InvoiceLine"
"""
type InvoiceLine_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [InvoiceLine!]!
}

"""
on_conflict condition type for table "InvoiceLine"
"""
input InvoiceLine_on_conflict {
  constraint: InvoiceLine_constraint!
  update_columns: [InvoiceLine_update_column!]! = []
  where: InvoiceLine_bool_exp
}

"""Ordering options when selecting data from "InvoiceLine"."""
input InvoiceLine_order_by {
  Invoice: Invoice_order_by
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  Track: Track_order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""primary key columns input for table: InvoiceLine"""
input InvoiceLine_pk_columns_input {
  InvoiceLineId: Int!
}

"""
select columns of table "InvoiceLine"
"""
enum InvoiceLine_select_column {
  """column name"""
  InvoiceId

  """column name"""
  InvoiceLineId

  """column name"""
  Quantity

  """column name"""
  TrackId

  """column name"""
  UnitPrice
}

"""
input type for updating data in table "InvoiceLine"
"""
input InvoiceLine_set_input {
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  TrackId: Int
  UnitPrice: numeric
}

"""aggregate stddev on columns"""
type InvoiceLine_stddev_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by stddev() on columns of table "InvoiceLine"
"""
input InvoiceLine_stddev_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate stddev_pop on columns"""
type InvoiceLine_stddev_pop_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by stddev_pop() on columns of table "InvoiceLine"
"""
input InvoiceLine_stddev_pop_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate stddev_samp on columns"""
type InvoiceLine_stddev_samp_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by stddev_samp() on columns of table "InvoiceLine"
"""
input InvoiceLine_stddev_samp_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
Streaming cursor of the table "InvoiceLine"
"""
input InvoiceLine_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: InvoiceLine_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input InvoiceLine_stream_cursor_value_input {
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  TrackId: Int
  UnitPrice: numeric
}

"""aggregate sum on columns"""
type InvoiceLine_sum_fields {
  InvoiceId: Int
  InvoiceLineId: Int
  Quantity: Int
  TrackId: Int
  UnitPrice: numeric
}

"""
order by sum() on columns of table "InvoiceLine"
"""
input InvoiceLine_sum_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
update columns of table "InvoiceLine"
"""
enum InvoiceLine_update_column {
  """column name"""
  InvoiceId

  """column name"""
  InvoiceLineId

  """column name"""
  Quantity

  """column name"""
  TrackId

  """column name"""
  UnitPrice
}

input InvoiceLine_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: InvoiceLine_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: InvoiceLine_set_input

  """filter the rows which have to be updated"""
  where: InvoiceLine_bool_exp!
}

"""aggregate var_pop on columns"""
type InvoiceLine_var_pop_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by var_pop() on columns of table "InvoiceLine"
"""
input InvoiceLine_var_pop_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate var_samp on columns"""
type InvoiceLine_var_samp_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by var_samp() on columns of table "InvoiceLine"
"""
input InvoiceLine_var_samp_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate variance on columns"""
type InvoiceLine_variance_fields {
  InvoiceId: Float
  InvoiceLineId: Float
  Quantity: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by variance() on columns of table "InvoiceLine"
"""
input InvoiceLine_variance_order_by {
  InvoiceId: order_by
  InvoiceLineId: order_by
  Quantity: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
aggregated selection of "Invoice"
"""
type Invoice_aggregate {
  aggregate: Invoice_aggregate_fields
  nodes: [Invoice!]!
}

input Invoice_aggregate_bool_exp {
  count: Invoice_aggregate_bool_exp_count
}

input Invoice_aggregate_bool_exp_count {
  arguments: [Invoice_select_column!]
  distinct: Boolean
  filter: Invoice_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "Invoice"
"""
type Invoice_aggregate_fields {
  avg: Invoice_avg_fields
  count(columns: [Invoice_select_column!], distinct: Boolean): Int!
  max: Invoice_max_fields
  min: Invoice_min_fields
  stddev: Invoice_stddev_fields
  stddev_pop: Invoice_stddev_pop_fields
  stddev_samp: Invoice_stddev_samp_fields
  sum: Invoice_sum_fields
  var_pop: Invoice_var_pop_fields
  var_samp: Invoice_var_samp_fields
  variance: Invoice_variance_fields
}

"""
order by aggregate values of table "Invoice"
"""
input Invoice_aggregate_order_by {
  avg: Invoice_avg_order_by
  count: order_by
  max: Invoice_max_order_by
  min: Invoice_min_order_by
  stddev: Invoice_stddev_order_by
  stddev_pop: Invoice_stddev_pop_order_by
  stddev_samp: Invoice_stddev_samp_order_by
  sum: Invoice_sum_order_by
  var_pop: Invoice_var_pop_order_by
  var_samp: Invoice_var_samp_order_by
  variance: Invoice_variance_order_by
}

"""
input type for inserting array relation for remote table "Invoice"
"""
input Invoice_arr_rel_insert_input {
  data: [Invoice_insert_input!]!

  """upsert condition"""
  on_conflict: Invoice_on_conflict
}

"""aggregate avg on columns"""
type Invoice_avg_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by avg() on columns of table "Invoice"
"""
input Invoice_avg_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""
Boolean expression to filter rows from the table "Invoice". All fields are combined with a logical 'AND'.
"""
input Invoice_bool_exp {
  BillingAddress: String_comparison_exp
  BillingCity: String_comparison_exp
  BillingCountry: String_comparison_exp
  BillingPostalCode: String_comparison_exp
  BillingState: String_comparison_exp
  Customer: Customer_bool_exp
  CustomerId: Int_comparison_exp
  InvoiceDate: timestamp_comparison_exp
  InvoiceId: Int_comparison_exp
  InvoiceLines: InvoiceLine_bool_exp
  InvoiceLines_aggregate: InvoiceLine_aggregate_bool_exp
  Total: numeric_comparison_exp
  _and: [Invoice_bool_exp!]
  _not: Invoice_bool_exp
  _or: [Invoice_bool_exp!]
}

"""
unique or primary key constraints on table "Invoice"
"""
enum Invoice_constraint {
  """
  unique or primary key constraint on columns "InvoiceId"
  """
  PK_Invoice
}

"""
input type for incrementing numeric columns in table "Invoice"
"""
input Invoice_inc_input {
  CustomerId: Int
  InvoiceId: Int
  Total: numeric
}

"""
input type for inserting data into table "Invoice"
"""
input Invoice_insert_input {
  BillingAddress: String
  BillingCity: String
  BillingCountry: String
  BillingPostalCode: String
  BillingState: String
  Customer: Customer_obj_rel_insert_input
  CustomerId: Int
  InvoiceDate: timestamp
  InvoiceId: Int
  InvoiceLines: InvoiceLine_arr_rel_insert_input
  Total: numeric
}

"""aggregate max on columns"""
type Invoice_max_fields {
  BillingAddress: String
  BillingCity: String
  BillingCountry: String
  BillingPostalCode: String
  BillingState: String
  CustomerId: Int
  InvoiceDate: timestamp
  InvoiceId: Int
  Total: numeric
}

"""
order by max() on columns of table "Invoice"
"""
input Invoice_max_order_by {
  BillingAddress: order_by
  BillingCity: order_by
  BillingCountry: order_by
  BillingPostalCode: order_by
  BillingState: order_by
  CustomerId: order_by
  InvoiceDate: order_by
  InvoiceId: order_by
  Total: order_by
}

"""aggregate min on columns"""
type Invoice_min_fields {
  BillingAddress: String
  BillingCity: String
  BillingCountry: String
  BillingPostalCode: String
  BillingState: String
  CustomerId: Int
  InvoiceDate: timestamp
  InvoiceId: Int
  Total: numeric
}

"""
order by min() on columns of table "Invoice"
"""
input Invoice_min_order_by {
  BillingAddress: order_by
  BillingCity: order_by
  BillingCountry: order_by
  BillingPostalCode: order_by
  BillingState: order_by
  CustomerId: order_by
  InvoiceDate: order_by
  InvoiceId: order_by
  Total: order_by
}

"""
response of any mutation on the table "Invoice"
"""
type Invoice_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Invoice!]!
}

"""
input type for inserting object relation for remote table "Invoice"
"""
input Invoice_obj_rel_insert_input {
  data: Invoice_insert_input!

  """upsert condition"""
  on_conflict: Invoice_on_conflict
}

"""
on_conflict condition type for table "Invoice"
"""
input Invoice_on_conflict {
  constraint: Invoice_constraint!
  update_columns: [Invoice_update_column!]! = []
  where: Invoice_bool_exp
}

"""Ordering options when selecting data from "Invoice"."""
input Invoice_order_by {
  BillingAddress: order_by
  BillingCity: order_by
  BillingCountry: order_by
  BillingPostalCode: order_by
  BillingState: order_by
  Customer: Customer_order_by
  CustomerId: order_by
  InvoiceDate: order_by
  InvoiceId: order_by
  InvoiceLines_aggregate: InvoiceLine_aggregate_order_by
  Total: order_by
}

"""primary key columns input for table: Invoice"""
input Invoice_pk_columns_input {
  InvoiceId: Int!
}

"""
select columns of table "Invoice"
"""
enum Invoice_select_column {
  """column name"""
  BillingAddress

  """column name"""
  BillingCity

  """column name"""
  BillingCountry

  """column name"""
  BillingPostalCode

  """column name"""
  BillingState

  """column name"""
  CustomerId

  """column name"""
  InvoiceDate

  """column name"""
  InvoiceId

  """column name"""
  Total
}

"""
input type for updating data in table "Invoice"
"""
input Invoice_set_input {
  BillingAddress: String
  BillingCity: String
  BillingCountry: String
  BillingPostalCode: String
  BillingState: String
  CustomerId: Int
  InvoiceDate: timestamp
  InvoiceId: Int
  Total: numeric
}

"""aggregate stddev on columns"""
type Invoice_stddev_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by stddev() on columns of table "Invoice"
"""
input Invoice_stddev_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""aggregate stddev_pop on columns"""
type Invoice_stddev_pop_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by stddev_pop() on columns of table "Invoice"
"""
input Invoice_stddev_pop_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""aggregate stddev_samp on columns"""
type Invoice_stddev_samp_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by stddev_samp() on columns of table "Invoice"
"""
input Invoice_stddev_samp_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""
Streaming cursor of the table "Invoice"
"""
input Invoice_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Invoice_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Invoice_stream_cursor_value_input {
  BillingAddress: String
  BillingCity: String
  BillingCountry: String
  BillingPostalCode: String
  BillingState: String
  CustomerId: Int
  InvoiceDate: timestamp
  InvoiceId: Int
  Total: numeric
}

"""aggregate sum on columns"""
type Invoice_sum_fields {
  CustomerId: Int
  InvoiceId: Int
  Total: numeric
}

"""
order by sum() on columns of table "Invoice"
"""
input Invoice_sum_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""
update columns of table "Invoice"
"""
enum Invoice_update_column {
  """column name"""
  BillingAddress

  """column name"""
  BillingCity

  """column name"""
  BillingCountry

  """column name"""
  BillingPostalCode

  """column name"""
  BillingState

  """column name"""
  CustomerId

  """column name"""
  InvoiceDate

  """column name"""
  InvoiceId

  """column name"""
  Total
}

input Invoice_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Invoice_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Invoice_set_input

  """filter the rows which have to be updated"""
  where: Invoice_bool_exp!
}

"""aggregate var_pop on columns"""
type Invoice_var_pop_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by var_pop() on columns of table "Invoice"
"""
input Invoice_var_pop_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""aggregate var_samp on columns"""
type Invoice_var_samp_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by var_samp() on columns of table "Invoice"
"""
input Invoice_var_samp_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""aggregate variance on columns"""
type Invoice_variance_fields {
  CustomerId: Float
  InvoiceId: Float
  Total: Float
}

"""
order by variance() on columns of table "Invoice"
"""
input Invoice_variance_order_by {
  CustomerId: order_by
  InvoiceId: order_by
  Total: order_by
}

"""
columns and relationships of "MediaType"
"""
type MediaType {
  MediaTypeId: Int!
  Name: String

  """An array relationship"""
  Tracks(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): [Track!]!

  """An aggregate relationship"""
  Tracks_aggregate(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): Track_aggregate!
}

"""
aggregated selection of "MediaType"
"""
type MediaType_aggregate {
  aggregate: MediaType_aggregate_fields
  nodes: [MediaType!]!
}

"""
aggregate fields of "MediaType"
"""
type MediaType_aggregate_fields {
  avg: MediaType_avg_fields
  count(columns: [MediaType_select_column!], distinct: Boolean): Int!
  max: MediaType_max_fields
  min: MediaType_min_fields
  stddev: MediaType_stddev_fields
  stddev_pop: MediaType_stddev_pop_fields
  stddev_samp: MediaType_stddev_samp_fields
  sum: MediaType_sum_fields
  var_pop: MediaType_var_pop_fields
  var_samp: MediaType_var_samp_fields
  variance: MediaType_variance_fields
}

"""aggregate avg on columns"""
type MediaType_avg_fields {
  MediaTypeId: Float
}

"""
Boolean expression to filter rows from the table "MediaType". All fields are combined with a logical 'AND'.
"""
input MediaType_bool_exp {
  MediaTypeId: Int_comparison_exp
  Name: String_comparison_exp
  Tracks: Track_bool_exp
  Tracks_aggregate: Track_aggregate_bool_exp
  _and: [MediaType_bool_exp!]
  _not: MediaType_bool_exp
  _or: [MediaType_bool_exp!]
}

"""
unique or primary key constraints on table "MediaType"
"""
enum MediaType_constraint {
  """
  unique or primary key constraint on columns "MediaTypeId"
  """
  PK_MediaType
}

"""
input type for incrementing numeric columns in table "MediaType"
"""
input MediaType_inc_input {
  MediaTypeId: Int
}

"""
input type for inserting data into table "MediaType"
"""
input MediaType_insert_input {
  MediaTypeId: Int
  Name: String
  Tracks: Track_arr_rel_insert_input
}

"""aggregate max on columns"""
type MediaType_max_fields {
  MediaTypeId: Int
  Name: String
}

"""aggregate min on columns"""
type MediaType_min_fields {
  MediaTypeId: Int
  Name: String
}

"""
response of any mutation on the table "MediaType"
"""
type MediaType_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [MediaType!]!
}

"""
input type for inserting object relation for remote table "MediaType"
"""
input MediaType_obj_rel_insert_input {
  data: MediaType_insert_input!

  """upsert condition"""
  on_conflict: MediaType_on_conflict
}

"""
on_conflict condition type for table "MediaType"
"""
input MediaType_on_conflict {
  constraint: MediaType_constraint!
  update_columns: [MediaType_update_column!]! = []
  where: MediaType_bool_exp
}

"""Ordering options when selecting data from "MediaType"."""
input MediaType_order_by {
  MediaTypeId: order_by
  Name: order_by
  Tracks_aggregate: Track_aggregate_order_by
}

"""primary key columns input for table: MediaType"""
input MediaType_pk_columns_input {
  MediaTypeId: Int!
}

"""
select columns of table "MediaType"
"""
enum MediaType_select_column {
  """column name"""
  MediaTypeId

  """column name"""
  Name
}

"""
input type for updating data in table "MediaType"
"""
input MediaType_set_input {
  MediaTypeId: Int
  Name: String
}

"""aggregate stddev on columns"""
type MediaType_stddev_fields {
  MediaTypeId: Float
}

"""aggregate stddev_pop on columns"""
type MediaType_stddev_pop_fields {
  MediaTypeId: Float
}

"""aggregate stddev_samp on columns"""
type MediaType_stddev_samp_fields {
  MediaTypeId: Float
}

"""
Streaming cursor of the table "MediaType"
"""
input MediaType_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: MediaType_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input MediaType_stream_cursor_value_input {
  MediaTypeId: Int
  Name: String
}

"""aggregate sum on columns"""
type MediaType_sum_fields {
  MediaTypeId: Int
}

"""
update columns of table "MediaType"
"""
enum MediaType_update_column {
  """column name"""
  MediaTypeId

  """column name"""
  Name
}

input MediaType_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: MediaType_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: MediaType_set_input

  """filter the rows which have to be updated"""
  where: MediaType_bool_exp!
}

"""aggregate var_pop on columns"""
type MediaType_var_pop_fields {
  MediaTypeId: Float
}

"""aggregate var_samp on columns"""
type MediaType_var_samp_fields {
  MediaTypeId: Float
}

"""aggregate variance on columns"""
type MediaType_variance_fields {
  MediaTypeId: Float
}

"""
columns and relationships of "Playlist"
"""
type Playlist {
  Name: String
  PlaylistId: Int!

  """An array relationship"""
  PlaylistTracks(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): [PlaylistTrack!]!

  """An aggregate relationship"""
  PlaylistTracks_aggregate(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): PlaylistTrack_aggregate!
}

"""
columns and relationships of "PlaylistTrack"
"""
type PlaylistTrack {
  """An object relationship"""
  Playlist: Playlist!
  PlaylistId: Int!

  """An object relationship"""
  Track: Track!
  TrackId: Int!
}

"""
aggregated selection of "PlaylistTrack"
"""
type PlaylistTrack_aggregate {
  aggregate: PlaylistTrack_aggregate_fields
  nodes: [PlaylistTrack!]!
}

input PlaylistTrack_aggregate_bool_exp {
  count: PlaylistTrack_aggregate_bool_exp_count
}

input PlaylistTrack_aggregate_bool_exp_count {
  arguments: [PlaylistTrack_select_column!]
  distinct: Boolean
  filter: PlaylistTrack_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "PlaylistTrack"
"""
type PlaylistTrack_aggregate_fields {
  avg: PlaylistTrack_avg_fields
  count(columns: [PlaylistTrack_select_column!], distinct: Boolean): Int!
  max: PlaylistTrack_max_fields
  min: PlaylistTrack_min_fields
  stddev: PlaylistTrack_stddev_fields
  stddev_pop: PlaylistTrack_stddev_pop_fields
  stddev_samp: PlaylistTrack_stddev_samp_fields
  sum: PlaylistTrack_sum_fields
  var_pop: PlaylistTrack_var_pop_fields
  var_samp: PlaylistTrack_var_samp_fields
  variance: PlaylistTrack_variance_fields
}

"""
order by aggregate values of table "PlaylistTrack"
"""
input PlaylistTrack_aggregate_order_by {
  avg: PlaylistTrack_avg_order_by
  count: order_by
  max: PlaylistTrack_max_order_by
  min: PlaylistTrack_min_order_by
  stddev: PlaylistTrack_stddev_order_by
  stddev_pop: PlaylistTrack_stddev_pop_order_by
  stddev_samp: PlaylistTrack_stddev_samp_order_by
  sum: PlaylistTrack_sum_order_by
  var_pop: PlaylistTrack_var_pop_order_by
  var_samp: PlaylistTrack_var_samp_order_by
  variance: PlaylistTrack_variance_order_by
}

"""
input type for inserting array relation for remote table "PlaylistTrack"
"""
input PlaylistTrack_arr_rel_insert_input {
  data: [PlaylistTrack_insert_input!]!

  """upsert condition"""
  on_conflict: PlaylistTrack_on_conflict
}

"""aggregate avg on columns"""
type PlaylistTrack_avg_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by avg() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_avg_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""
Boolean expression to filter rows from the table "PlaylistTrack". All fields are combined with a logical 'AND'.
"""
input PlaylistTrack_bool_exp {
  Playlist: Playlist_bool_exp
  PlaylistId: Int_comparison_exp
  Track: Track_bool_exp
  TrackId: Int_comparison_exp
  _and: [PlaylistTrack_bool_exp!]
  _not: PlaylistTrack_bool_exp
  _or: [PlaylistTrack_bool_exp!]
}

"""
unique or primary key constraints on table "PlaylistTrack"
"""
enum PlaylistTrack_constraint {
  """
  unique or primary key constraint on columns "PlaylistId", "TrackId"
  """
  PK_PlaylistTrack
}

"""
input type for incrementing numeric columns in table "PlaylistTrack"
"""
input PlaylistTrack_inc_input {
  PlaylistId: Int
  TrackId: Int
}

"""
input type for inserting data into table "PlaylistTrack"
"""
input PlaylistTrack_insert_input {
  Playlist: Playlist_obj_rel_insert_input
  PlaylistId: Int
  Track: Track_obj_rel_insert_input
  TrackId: Int
}

"""aggregate max on columns"""
type PlaylistTrack_max_fields {
  PlaylistId: Int
  TrackId: Int
}

"""
order by max() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_max_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""aggregate min on columns"""
type PlaylistTrack_min_fields {
  PlaylistId: Int
  TrackId: Int
}

"""
order by min() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_min_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""
response of any mutation on the table "PlaylistTrack"
"""
type PlaylistTrack_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [PlaylistTrack!]!
}

"""
on_conflict condition type for table "PlaylistTrack"
"""
input PlaylistTrack_on_conflict {
  constraint: PlaylistTrack_constraint!
  update_columns: [PlaylistTrack_update_column!]! = []
  where: PlaylistTrack_bool_exp
}

"""Ordering options when selecting data from "PlaylistTrack"."""
input PlaylistTrack_order_by {
  Playlist: Playlist_order_by
  PlaylistId: order_by
  Track: Track_order_by
  TrackId: order_by
}

"""primary key columns input for table: PlaylistTrack"""
input PlaylistTrack_pk_columns_input {
  PlaylistId: Int!
  TrackId: Int!
}

"""
select columns of table "PlaylistTrack"
"""
enum PlaylistTrack_select_column {
  """column name"""
  PlaylistId

  """column name"""
  TrackId
}

"""
input type for updating data in table "PlaylistTrack"
"""
input PlaylistTrack_set_input {
  PlaylistId: Int
  TrackId: Int
}

"""aggregate stddev on columns"""
type PlaylistTrack_stddev_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by stddev() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_stddev_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""aggregate stddev_pop on columns"""
type PlaylistTrack_stddev_pop_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by stddev_pop() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_stddev_pop_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""aggregate stddev_samp on columns"""
type PlaylistTrack_stddev_samp_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by stddev_samp() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_stddev_samp_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""
Streaming cursor of the table "PlaylistTrack"
"""
input PlaylistTrack_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: PlaylistTrack_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input PlaylistTrack_stream_cursor_value_input {
  PlaylistId: Int
  TrackId: Int
}

"""aggregate sum on columns"""
type PlaylistTrack_sum_fields {
  PlaylistId: Int
  TrackId: Int
}

"""
order by sum() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_sum_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""
update columns of table "PlaylistTrack"
"""
enum PlaylistTrack_update_column {
  """column name"""
  PlaylistId

  """column name"""
  TrackId
}

input PlaylistTrack_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: PlaylistTrack_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: PlaylistTrack_set_input

  """filter the rows which have to be updated"""
  where: PlaylistTrack_bool_exp!
}

"""aggregate var_pop on columns"""
type PlaylistTrack_var_pop_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by var_pop() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_var_pop_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""aggregate var_samp on columns"""
type PlaylistTrack_var_samp_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by var_samp() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_var_samp_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""aggregate variance on columns"""
type PlaylistTrack_variance_fields {
  PlaylistId: Float
  TrackId: Float
}

"""
order by variance() on columns of table "PlaylistTrack"
"""
input PlaylistTrack_variance_order_by {
  PlaylistId: order_by
  TrackId: order_by
}

"""
aggregated selection of "Playlist"
"""
type Playlist_aggregate {
  aggregate: Playlist_aggregate_fields
  nodes: [Playlist!]!
}

"""
aggregate fields of "Playlist"
"""
type Playlist_aggregate_fields {
  avg: Playlist_avg_fields
  count(columns: [Playlist_select_column!], distinct: Boolean): Int!
  max: Playlist_max_fields
  min: Playlist_min_fields
  stddev: Playlist_stddev_fields
  stddev_pop: Playlist_stddev_pop_fields
  stddev_samp: Playlist_stddev_samp_fields
  sum: Playlist_sum_fields
  var_pop: Playlist_var_pop_fields
  var_samp: Playlist_var_samp_fields
  variance: Playlist_variance_fields
}

"""aggregate avg on columns"""
type Playlist_avg_fields {
  PlaylistId: Float
}

"""
Boolean expression to filter rows from the table "Playlist". All fields are combined with a logical 'AND'.
"""
input Playlist_bool_exp {
  Name: String_comparison_exp
  PlaylistId: Int_comparison_exp
  PlaylistTracks: PlaylistTrack_bool_exp
  PlaylistTracks_aggregate: PlaylistTrack_aggregate_bool_exp
  _and: [Playlist_bool_exp!]
  _not: Playlist_bool_exp
  _or: [Playlist_bool_exp!]
}

"""
unique or primary key constraints on table "Playlist"
"""
enum Playlist_constraint {
  """
  unique or primary key constraint on columns "PlaylistId"
  """
  PK_Playlist
}

"""
input type for incrementing numeric columns in table "Playlist"
"""
input Playlist_inc_input {
  PlaylistId: Int
}

"""
input type for inserting data into table "Playlist"
"""
input Playlist_insert_input {
  Name: String
  PlaylistId: Int
  PlaylistTracks: PlaylistTrack_arr_rel_insert_input
}

"""aggregate max on columns"""
type Playlist_max_fields {
  Name: String
  PlaylistId: Int
}

"""aggregate min on columns"""
type Playlist_min_fields {
  Name: String
  PlaylistId: Int
}

"""
response of any mutation on the table "Playlist"
"""
type Playlist_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Playlist!]!
}

"""
input type for inserting object relation for remote table "Playlist"
"""
input Playlist_obj_rel_insert_input {
  data: Playlist_insert_input!

  """upsert condition"""
  on_conflict: Playlist_on_conflict
}

"""
on_conflict condition type for table "Playlist"
"""
input Playlist_on_conflict {
  constraint: Playlist_constraint!
  update_columns: [Playlist_update_column!]! = []
  where: Playlist_bool_exp
}

"""Ordering options when selecting data from "Playlist"."""
input Playlist_order_by {
  Name: order_by
  PlaylistId: order_by
  PlaylistTracks_aggregate: PlaylistTrack_aggregate_order_by
}

"""primary key columns input for table: Playlist"""
input Playlist_pk_columns_input {
  PlaylistId: Int!
}

"""
select columns of table "Playlist"
"""
enum Playlist_select_column {
  """column name"""
  Name

  """column name"""
  PlaylistId
}

"""
input type for updating data in table "Playlist"
"""
input Playlist_set_input {
  Name: String
  PlaylistId: Int
}

"""aggregate stddev on columns"""
type Playlist_stddev_fields {
  PlaylistId: Float
}

"""aggregate stddev_pop on columns"""
type Playlist_stddev_pop_fields {
  PlaylistId: Float
}

"""aggregate stddev_samp on columns"""
type Playlist_stddev_samp_fields {
  PlaylistId: Float
}

"""
Streaming cursor of the table "Playlist"
"""
input Playlist_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Playlist_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Playlist_stream_cursor_value_input {
  Name: String
  PlaylistId: Int
}

"""aggregate sum on columns"""
type Playlist_sum_fields {
  PlaylistId: Int
}

"""
update columns of table "Playlist"
"""
enum Playlist_update_column {
  """column name"""
  Name

  """column name"""
  PlaylistId
}

input Playlist_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Playlist_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Playlist_set_input

  """filter the rows which have to be updated"""
  where: Playlist_bool_exp!
}

"""aggregate var_pop on columns"""
type Playlist_var_pop_fields {
  PlaylistId: Float
}

"""aggregate var_samp on columns"""
type Playlist_var_samp_fields {
  PlaylistId: Float
}

"""aggregate variance on columns"""
type Playlist_variance_fields {
  PlaylistId: Float
}

"""
Boolean expression to compare columns of type "String". All fields are combined with logical 'AND'.
"""
input String_comparison_exp {
  _eq: String
  _gt: String
  _gte: String

  """does the column match the given case-insensitive pattern"""
  _ilike: String
  _in: [String!]

  """
  does the column match the given POSIX regular expression, case insensitive
  """
  _iregex: String
  _is_null: Boolean

  """does the column match the given pattern"""
  _like: String
  _lt: String
  _lte: String
  _neq: String

  """does the column NOT match the given case-insensitive pattern"""
  _nilike: String
  _nin: [String!]

  """
  does the column NOT match the given POSIX regular expression, case insensitive
  """
  _niregex: String

  """does the column NOT match the given pattern"""
  _nlike: String

  """
  does the column NOT match the given POSIX regular expression, case sensitive
  """
  _nregex: String

  """does the column NOT match the given SQL regular expression"""
  _nsimilar: String

  """
  does the column match the given POSIX regular expression, case sensitive
  """
  _regex: String

  """does the column match the given SQL regular expression"""
  _similar: String
}

"""
columns and relationships of "Track"
"""
type Track {
  """An object relationship"""
  Album: Album
  AlbumId: Int
  Bytes: Int
  Composer: String

  """An object relationship"""
  Genre: Genre
  GenreId: Int

  """An array relationship"""
  InvoiceLines(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): [InvoiceLine!]!

  """An aggregate relationship"""
  InvoiceLines_aggregate(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): InvoiceLine_aggregate!

  """An object relationship"""
  MediaType: MediaType!
  MediaTypeId: Int!
  Milliseconds: Int!
  Name: String!

  """An array relationship"""
  PlaylistTracks(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): [PlaylistTrack!]!

  """An aggregate relationship"""
  PlaylistTracks_aggregate(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): PlaylistTrack_aggregate!
  TrackId: Int!
  UnitPrice: numeric!
}

"""
aggregated selection of "Track"
"""
type Track_aggregate {
  aggregate: Track_aggregate_fields
  nodes: [Track!]!
}

input Track_aggregate_bool_exp {
  count: Track_aggregate_bool_exp_count
}

input Track_aggregate_bool_exp_count {
  arguments: [Track_select_column!]
  distinct: Boolean
  filter: Track_bool_exp
  predicate: Int_comparison_exp!
}

"""
aggregate fields of "Track"
"""
type Track_aggregate_fields {
  avg: Track_avg_fields
  count(columns: [Track_select_column!], distinct: Boolean): Int!
  max: Track_max_fields
  min: Track_min_fields
  stddev: Track_stddev_fields
  stddev_pop: Track_stddev_pop_fields
  stddev_samp: Track_stddev_samp_fields
  sum: Track_sum_fields
  var_pop: Track_var_pop_fields
  var_samp: Track_var_samp_fields
  variance: Track_variance_fields
}

"""
order by aggregate values of table "Track"
"""
input Track_aggregate_order_by {
  avg: Track_avg_order_by
  count: order_by
  max: Track_max_order_by
  min: Track_min_order_by
  stddev: Track_stddev_order_by
  stddev_pop: Track_stddev_pop_order_by
  stddev_samp: Track_stddev_samp_order_by
  sum: Track_sum_order_by
  var_pop: Track_var_pop_order_by
  var_samp: Track_var_samp_order_by
  variance: Track_variance_order_by
}

"""
input type for inserting array relation for remote table "Track"
"""
input Track_arr_rel_insert_input {
  data: [Track_insert_input!]!

  """upsert condition"""
  on_conflict: Track_on_conflict
}

"""aggregate avg on columns"""
type Track_avg_fields {
  AlbumId: Float
  Bytes: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by avg() on columns of table "Track"
"""
input Track_avg_order_by {
  AlbumId: order_by
  Bytes: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
Boolean expression to filter rows from the table "Track". All fields are combined with a logical 'AND'.
"""
input Track_bool_exp {
  Album: Album_bool_exp
  AlbumId: Int_comparison_exp
  Bytes: Int_comparison_exp
  Composer: String_comparison_exp
  Genre: Genre_bool_exp
  GenreId: Int_comparison_exp
  InvoiceLines: InvoiceLine_bool_exp
  InvoiceLines_aggregate: InvoiceLine_aggregate_bool_exp
  MediaType: MediaType_bool_exp
  MediaTypeId: Int_comparison_exp
  Milliseconds: Int_comparison_exp
  Name: String_comparison_exp
  PlaylistTracks: PlaylistTrack_bool_exp
  PlaylistTracks_aggregate: PlaylistTrack_aggregate_bool_exp
  TrackId: Int_comparison_exp
  UnitPrice: numeric_comparison_exp
  _and: [Track_bool_exp!]
  _not: Track_bool_exp
  _or: [Track_bool_exp!]
}

"""
unique or primary key constraints on table "Track"
"""
enum Track_constraint {
  """
  unique or primary key constraint on columns "TrackId"
  """
  PK_Track
}

"""
input type for incrementing numeric columns in table "Track"
"""
input Track_inc_input {
  AlbumId: Int
  Bytes: Int
  GenreId: Int
  MediaTypeId: Int
  Milliseconds: Int
  TrackId: Int
  UnitPrice: numeric
}

"""
input type for inserting data into table "Track"
"""
input Track_insert_input {
  Album: Album_obj_rel_insert_input
  AlbumId: Int
  Bytes: Int
  Composer: String
  Genre: Genre_obj_rel_insert_input
  GenreId: Int
  InvoiceLines: InvoiceLine_arr_rel_insert_input
  MediaType: MediaType_obj_rel_insert_input
  MediaTypeId: Int
  Milliseconds: Int
  Name: String
  PlaylistTracks: PlaylistTrack_arr_rel_insert_input
  TrackId: Int
  UnitPrice: numeric
}

"""aggregate max on columns"""
type Track_max_fields {
  AlbumId: Int
  Bytes: Int
  Composer: String
  GenreId: Int
  MediaTypeId: Int
  Milliseconds: Int
  Name: String
  TrackId: Int
  UnitPrice: numeric
}

"""
order by max() on columns of table "Track"
"""
input Track_max_order_by {
  AlbumId: order_by
  Bytes: order_by
  Composer: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  Name: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate min on columns"""
type Track_min_fields {
  AlbumId: Int
  Bytes: Int
  Composer: String
  GenreId: Int
  MediaTypeId: Int
  Milliseconds: Int
  Name: String
  TrackId: Int
  UnitPrice: numeric
}

"""
order by min() on columns of table "Track"
"""
input Track_min_order_by {
  AlbumId: order_by
  Bytes: order_by
  Composer: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  Name: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
response of any mutation on the table "Track"
"""
type Track_mutation_response {
  """number of rows affected by the mutation"""
  affected_rows: Int!

  """data from the rows affected by the mutation"""
  returning: [Track!]!
}

"""
input type for inserting object relation for remote table "Track"
"""
input Track_obj_rel_insert_input {
  data: Track_insert_input!

  """upsert condition"""
  on_conflict: Track_on_conflict
}

"""
on_conflict condition type for table "Track"
"""
input Track_on_conflict {
  constraint: Track_constraint!
  update_columns: [Track_update_column!]! = []
  where: Track_bool_exp
}

"""Ordering options when selecting data from "Track"."""
input Track_order_by {
  Album: Album_order_by
  AlbumId: order_by
  Bytes: order_by
  Composer: order_by
  Genre: Genre_order_by
  GenreId: order_by
  InvoiceLines_aggregate: InvoiceLine_aggregate_order_by
  MediaType: MediaType_order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  Name: order_by
  PlaylistTracks_aggregate: PlaylistTrack_aggregate_order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""primary key columns input for table: Track"""
input Track_pk_columns_input {
  TrackId: Int!
}

"""
select columns of table "Track"
"""
enum Track_select_column {
  """column name"""
  AlbumId

  """column name"""
  Bytes

  """column name"""
  Composer

  """column name"""
  GenreId

  """column name"""
  MediaTypeId

  """column name"""
  Milliseconds

  """column name"""
  Name

  """column name"""
  TrackId

  """column name"""
  UnitPrice
}

"""
input type for updating data in table "Track"
"""
input Track_set_input {
  AlbumId: Int
  Bytes: Int
  Composer: String
  GenreId: Int
  MediaTypeId: Int
  Milliseconds: Int
  Name: String
  TrackId: Int
  UnitPrice: numeric
}

"""aggregate stddev on columns"""
type Track_stddev_fields {
  AlbumId: Float
  Bytes: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by stddev() on columns of table "Track"
"""
input Track_stddev_order_by {
  AlbumId: order_by
  Bytes: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate stddev_pop on columns"""
type Track_stddev_pop_fields {
  AlbumId: Float
  Bytes: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by stddev_pop() on columns of table "Track"
"""
input Track_stddev_pop_order_by {
  AlbumId: order_by
  Bytes: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate stddev_samp on columns"""
type Track_stddev_samp_fields {
  AlbumId: Float
  Bytes: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by stddev_samp() on columns of table "Track"
"""
input Track_stddev_samp_order_by {
  AlbumId: order_by
  Bytes: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
Streaming cursor of the table "Track"
"""
input Track_stream_cursor_input {
  """Stream column input with initial value"""
  initial_value: Track_stream_cursor_value_input!

  """cursor ordering"""
  ordering: cursor_ordering
}

"""Initial value of the column from where the streaming should start"""
input Track_stream_cursor_value_input {
  AlbumId: Int
  Bytes: Int
  Composer: String
  GenreId: Int
  MediaTypeId: Int
  Milliseconds: Int
  Name: String
  TrackId: Int
  UnitPrice: numeric
}

"""aggregate sum on columns"""
type Track_sum_fields {
  AlbumId: Int
  Bytes: Int
  GenreId: Int
  MediaTypeId: Int
  Milliseconds: Int
  TrackId: Int
  UnitPrice: numeric
}

"""
order by sum() on columns of table "Track"
"""
input Track_sum_order_by {
  AlbumId: order_by
  Bytes: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""
update columns of table "Track"
"""
enum Track_update_column {
  """column name"""
  AlbumId

  """column name"""
  Bytes

  """column name"""
  Composer

  """column name"""
  GenreId

  """column name"""
  MediaTypeId

  """column name"""
  Milliseconds

  """column name"""
  Name

  """column name"""
  TrackId

  """column name"""
  UnitPrice
}

input Track_updates {
  """increments the numeric columns with given value of the filtered values"""
  _inc: Track_inc_input

  """sets the columns of the filtered rows to the given values"""
  _set: Track_set_input

  """filter the rows which have to be updated"""
  where: Track_bool_exp!
}

"""aggregate var_pop on columns"""
type Track_var_pop_fields {
  AlbumId: Float
  Bytes: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by var_pop() on columns of table "Track"
"""
input Track_var_pop_order_by {
  AlbumId: order_by
  Bytes: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate var_samp on columns"""
type Track_var_samp_fields {
  AlbumId: Float
  Bytes: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by var_samp() on columns of table "Track"
"""
input Track_var_samp_order_by {
  AlbumId: order_by
  Bytes: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""aggregate variance on columns"""
type Track_variance_fields {
  AlbumId: Float
  Bytes: Float
  GenreId: Float
  MediaTypeId: Float
  Milliseconds: Float
  TrackId: Float
  UnitPrice: Float
}

"""
order by variance() on columns of table "Track"
"""
input Track_variance_order_by {
  AlbumId: order_by
  Bytes: order_by
  GenreId: order_by
  MediaTypeId: order_by
  Milliseconds: order_by
  TrackId: order_by
  UnitPrice: order_by
}

"""ordering argument of a cursor"""
enum cursor_ordering {
  """ascending ordering of the cursor"""
  ASC

  """descending ordering of the cursor"""
  DESC
}

"""mutation root"""
type mutation_root {
  """
  delete data from the table: "Album"
  """
  delete_Album(
    """filter the rows which have to be deleted"""
    where: Album_bool_exp!
  ): Album_mutation_response

  """
  delete single row from the table: "Album"
  """
  delete_Album_by_pk(AlbumId: Int!): Album

  """
  delete data from the table: "Artist"
  """
  delete_Artist(
    """filter the rows which have to be deleted"""
    where: Artist_bool_exp!
  ): Artist_mutation_response

  """
  delete single row from the table: "Artist"
  """
  delete_Artist_by_pk(ArtistId: Int!): Artist

  """
  delete data from the table: "Customer"
  """
  delete_Customer(
    """filter the rows which have to be deleted"""
    where: Customer_bool_exp!
  ): Customer_mutation_response

  """
  delete single row from the table: "Customer"
  """
  delete_Customer_by_pk(CustomerId: Int!): Customer

  """
  delete data from the table: "Employee"
  """
  delete_Employee(
    """filter the rows which have to be deleted"""
    where: Employee_bool_exp!
  ): Employee_mutation_response

  """
  delete single row from the table: "Employee"
  """
  delete_Employee_by_pk(EmployeeId: Int!): Employee

  """
  delete data from the table: "Genre"
  """
  delete_Genre(
    """filter the rows which have to be deleted"""
    where: Genre_bool_exp!
  ): Genre_mutation_response

  """
  delete single row from the table: "Genre"
  """
  delete_Genre_by_pk(GenreId: Int!): Genre

  """
  delete data from the table: "Invoice"
  """
  delete_Invoice(
    """filter the rows which have to be deleted"""
    where: Invoice_bool_exp!
  ): Invoice_mutation_response

  """
  delete data from the table: "InvoiceLine"
  """
  delete_InvoiceLine(
    """filter the rows which have to be deleted"""
    where: InvoiceLine_bool_exp!
  ): InvoiceLine_mutation_response

  """
  delete single row from the table: "InvoiceLine"
  """
  delete_InvoiceLine_by_pk(InvoiceLineId: Int!): InvoiceLine

  """
  delete single row from the table: "Invoice"
  """
  delete_Invoice_by_pk(InvoiceId: Int!): Invoice

  """
  delete data from the table: "MediaType"
  """
  delete_MediaType(
    """filter the rows which have to be deleted"""
    where: MediaType_bool_exp!
  ): MediaType_mutation_response

  """
  delete single row from the table: "MediaType"
  """
  delete_MediaType_by_pk(MediaTypeId: Int!): MediaType

  """
  delete data from the table: "Playlist"
  """
  delete_Playlist(
    """filter the rows which have to be deleted"""
    where: Playlist_bool_exp!
  ): Playlist_mutation_response

  """
  delete data from the table: "PlaylistTrack"
  """
  delete_PlaylistTrack(
    """filter the rows which have to be deleted"""
    where: PlaylistTrack_bool_exp!
  ): PlaylistTrack_mutation_response

  """
  delete single row from the table: "PlaylistTrack"
  """
  delete_PlaylistTrack_by_pk(PlaylistId: Int!, TrackId: Int!): PlaylistTrack

  """
  delete single row from the table: "Playlist"
  """
  delete_Playlist_by_pk(PlaylistId: Int!): Playlist

  """
  delete data from the table: "Track"
  """
  delete_Track(
    """filter the rows which have to be deleted"""
    where: Track_bool_exp!
  ): Track_mutation_response

  """
  delete single row from the table: "Track"
  """
  delete_Track_by_pk(TrackId: Int!): Track

  """
  insert data into the table: "Album"
  """
  insert_Album(
    """the rows to be inserted"""
    objects: [Album_insert_input!]!

    """upsert condition"""
    on_conflict: Album_on_conflict
  ): Album_mutation_response

  """
  insert a single row into the table: "Album"
  """
  insert_Album_one(
    """the row to be inserted"""
    object: Album_insert_input!

    """upsert condition"""
    on_conflict: Album_on_conflict
  ): Album

  """
  insert data into the table: "Artist"
  """
  insert_Artist(
    """the rows to be inserted"""
    objects: [Artist_insert_input!]!

    """upsert condition"""
    on_conflict: Artist_on_conflict
  ): Artist_mutation_response

  """
  insert a single row into the table: "Artist"
  """
  insert_Artist_one(
    """the row to be inserted"""
    object: Artist_insert_input!

    """upsert condition"""
    on_conflict: Artist_on_conflict
  ): Artist

  """
  insert data into the table: "Customer"
  """
  insert_Customer(
    """the rows to be inserted"""
    objects: [Customer_insert_input!]!

    """upsert condition"""
    on_conflict: Customer_on_conflict
  ): Customer_mutation_response

  """
  insert a single row into the table: "Customer"
  """
  insert_Customer_one(
    """the row to be inserted"""
    object: Customer_insert_input!

    """upsert condition"""
    on_conflict: Customer_on_conflict
  ): Customer

  """
  insert data into the table: "Employee"
  """
  insert_Employee(
    """the rows to be inserted"""
    objects: [Employee_insert_input!]!

    """upsert condition"""
    on_conflict: Employee_on_conflict
  ): Employee_mutation_response

  """
  insert a single row into the table: "Employee"
  """
  insert_Employee_one(
    """the row to be inserted"""
    object: Employee_insert_input!

    """upsert condition"""
    on_conflict: Employee_on_conflict
  ): Employee

  """
  insert data into the table: "Genre"
  """
  insert_Genre(
    """the rows to be inserted"""
    objects: [Genre_insert_input!]!

    """upsert condition"""
    on_conflict: Genre_on_conflict
  ): Genre_mutation_response

  """
  insert a single row into the table: "Genre"
  """
  insert_Genre_one(
    """the row to be inserted"""
    object: Genre_insert_input!

    """upsert condition"""
    on_conflict: Genre_on_conflict
  ): Genre

  """
  insert data into the table: "Invoice"
  """
  insert_Invoice(
    """the rows to be inserted"""
    objects: [Invoice_insert_input!]!

    """upsert condition"""
    on_conflict: Invoice_on_conflict
  ): Invoice_mutation_response

  """
  insert data into the table: "InvoiceLine"
  """
  insert_InvoiceLine(
    """the rows to be inserted"""
    objects: [InvoiceLine_insert_input!]!

    """upsert condition"""
    on_conflict: InvoiceLine_on_conflict
  ): InvoiceLine_mutation_response

  """
  insert a single row into the table: "InvoiceLine"
  """
  insert_InvoiceLine_one(
    """the row to be inserted"""
    object: InvoiceLine_insert_input!

    """upsert condition"""
    on_conflict: InvoiceLine_on_conflict
  ): InvoiceLine

  """
  insert a single row into the table: "Invoice"
  """
  insert_Invoice_one(
    """the row to be inserted"""
    object: Invoice_insert_input!

    """upsert condition"""
    on_conflict: Invoice_on_conflict
  ): Invoice

  """
  insert data into the table: "MediaType"
  """
  insert_MediaType(
    """the rows to be inserted"""
    objects: [MediaType_insert_input!]!

    """upsert condition"""
    on_conflict: MediaType_on_conflict
  ): MediaType_mutation_response

  """
  insert a single row into the table: "MediaType"
  """
  insert_MediaType_one(
    """the row to be inserted"""
    object: MediaType_insert_input!

    """upsert condition"""
    on_conflict: MediaType_on_conflict
  ): MediaType

  """
  insert data into the table: "Playlist"
  """
  insert_Playlist(
    """the rows to be inserted"""
    objects: [Playlist_insert_input!]!

    """upsert condition"""
    on_conflict: Playlist_on_conflict
  ): Playlist_mutation_response

  """
  insert data into the table: "PlaylistTrack"
  """
  insert_PlaylistTrack(
    """the rows to be inserted"""
    objects: [PlaylistTrack_insert_input!]!

    """upsert condition"""
    on_conflict: PlaylistTrack_on_conflict
  ): PlaylistTrack_mutation_response

  """
  insert a single row into the table: "PlaylistTrack"
  """
  insert_PlaylistTrack_one(
    """the row to be inserted"""
    object: PlaylistTrack_insert_input!

    """upsert condition"""
    on_conflict: PlaylistTrack_on_conflict
  ): PlaylistTrack

  """
  insert a single row into the table: "Playlist"
  """
  insert_Playlist_one(
    """the row to be inserted"""
    object: Playlist_insert_input!

    """upsert condition"""
    on_conflict: Playlist_on_conflict
  ): Playlist

  """
  insert data into the table: "Track"
  """
  insert_Track(
    """the rows to be inserted"""
    objects: [Track_insert_input!]!

    """upsert condition"""
    on_conflict: Track_on_conflict
  ): Track_mutation_response

  """
  insert a single row into the table: "Track"
  """
  insert_Track_one(
    """the row to be inserted"""
    object: Track_insert_input!

    """upsert condition"""
    on_conflict: Track_on_conflict
  ): Track

  """
  update data of the table: "Album"
  """
  update_Album(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Album_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Album_set_input

    """filter the rows which have to be updated"""
    where: Album_bool_exp!
  ): Album_mutation_response

  """
  update single row of the table: "Album"
  """
  update_Album_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Album_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Album_set_input
    pk_columns: Album_pk_columns_input!
  ): Album

  """
  update multiples rows of table: "Album"
  """
  update_Album_many(
    """updates to execute, in order"""
    updates: [Album_updates!]!
  ): [Album_mutation_response]

  """
  update data of the table: "Artist"
  """
  update_Artist(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Artist_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Artist_set_input

    """filter the rows which have to be updated"""
    where: Artist_bool_exp!
  ): Artist_mutation_response

  """
  update single row of the table: "Artist"
  """
  update_Artist_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Artist_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Artist_set_input
    pk_columns: Artist_pk_columns_input!
  ): Artist

  """
  update multiples rows of table: "Artist"
  """
  update_Artist_many(
    """updates to execute, in order"""
    updates: [Artist_updates!]!
  ): [Artist_mutation_response]

  """
  update data of the table: "Customer"
  """
  update_Customer(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Customer_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Customer_set_input

    """filter the rows which have to be updated"""
    where: Customer_bool_exp!
  ): Customer_mutation_response

  """
  update single row of the table: "Customer"
  """
  update_Customer_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Customer_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Customer_set_input
    pk_columns: Customer_pk_columns_input!
  ): Customer

  """
  update multiples rows of table: "Customer"
  """
  update_Customer_many(
    """updates to execute, in order"""
    updates: [Customer_updates!]!
  ): [Customer_mutation_response]

  """
  update data of the table: "Employee"
  """
  update_Employee(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Employee_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Employee_set_input

    """filter the rows which have to be updated"""
    where: Employee_bool_exp!
  ): Employee_mutation_response

  """
  update single row of the table: "Employee"
  """
  update_Employee_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Employee_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Employee_set_input
    pk_columns: Employee_pk_columns_input!
  ): Employee

  """
  update multiples rows of table: "Employee"
  """
  update_Employee_many(
    """updates to execute, in order"""
    updates: [Employee_updates!]!
  ): [Employee_mutation_response]

  """
  update data of the table: "Genre"
  """
  update_Genre(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Genre_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Genre_set_input

    """filter the rows which have to be updated"""
    where: Genre_bool_exp!
  ): Genre_mutation_response

  """
  update single row of the table: "Genre"
  """
  update_Genre_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Genre_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Genre_set_input
    pk_columns: Genre_pk_columns_input!
  ): Genre

  """
  update multiples rows of table: "Genre"
  """
  update_Genre_many(
    """updates to execute, in order"""
    updates: [Genre_updates!]!
  ): [Genre_mutation_response]

  """
  update data of the table: "Invoice"
  """
  update_Invoice(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Invoice_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Invoice_set_input

    """filter the rows which have to be updated"""
    where: Invoice_bool_exp!
  ): Invoice_mutation_response

  """
  update data of the table: "InvoiceLine"
  """
  update_InvoiceLine(
    """increments the numeric columns with given value of the filtered values"""
    _inc: InvoiceLine_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: InvoiceLine_set_input

    """filter the rows which have to be updated"""
    where: InvoiceLine_bool_exp!
  ): InvoiceLine_mutation_response

  """
  update single row of the table: "InvoiceLine"
  """
  update_InvoiceLine_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: InvoiceLine_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: InvoiceLine_set_input
    pk_columns: InvoiceLine_pk_columns_input!
  ): InvoiceLine

  """
  update multiples rows of table: "InvoiceLine"
  """
  update_InvoiceLine_many(
    """updates to execute, in order"""
    updates: [InvoiceLine_updates!]!
  ): [InvoiceLine_mutation_response]

  """
  update single row of the table: "Invoice"
  """
  update_Invoice_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Invoice_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Invoice_set_input
    pk_columns: Invoice_pk_columns_input!
  ): Invoice

  """
  update multiples rows of table: "Invoice"
  """
  update_Invoice_many(
    """updates to execute, in order"""
    updates: [Invoice_updates!]!
  ): [Invoice_mutation_response]

  """
  update data of the table: "MediaType"
  """
  update_MediaType(
    """increments the numeric columns with given value of the filtered values"""
    _inc: MediaType_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: MediaType_set_input

    """filter the rows which have to be updated"""
    where: MediaType_bool_exp!
  ): MediaType_mutation_response

  """
  update single row of the table: "MediaType"
  """
  update_MediaType_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: MediaType_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: MediaType_set_input
    pk_columns: MediaType_pk_columns_input!
  ): MediaType

  """
  update multiples rows of table: "MediaType"
  """
  update_MediaType_many(
    """updates to execute, in order"""
    updates: [MediaType_updates!]!
  ): [MediaType_mutation_response]

  """
  update data of the table: "Playlist"
  """
  update_Playlist(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Playlist_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Playlist_set_input

    """filter the rows which have to be updated"""
    where: Playlist_bool_exp!
  ): Playlist_mutation_response

  """
  update data of the table: "PlaylistTrack"
  """
  update_PlaylistTrack(
    """increments the numeric columns with given value of the filtered values"""
    _inc: PlaylistTrack_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: PlaylistTrack_set_input

    """filter the rows which have to be updated"""
    where: PlaylistTrack_bool_exp!
  ): PlaylistTrack_mutation_response

  """
  update single row of the table: "PlaylistTrack"
  """
  update_PlaylistTrack_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: PlaylistTrack_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: PlaylistTrack_set_input
    pk_columns: PlaylistTrack_pk_columns_input!
  ): PlaylistTrack

  """
  update multiples rows of table: "PlaylistTrack"
  """
  update_PlaylistTrack_many(
    """updates to execute, in order"""
    updates: [PlaylistTrack_updates!]!
  ): [PlaylistTrack_mutation_response]

  """
  update single row of the table: "Playlist"
  """
  update_Playlist_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Playlist_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Playlist_set_input
    pk_columns: Playlist_pk_columns_input!
  ): Playlist

  """
  update multiples rows of table: "Playlist"
  """
  update_Playlist_many(
    """updates to execute, in order"""
    updates: [Playlist_updates!]!
  ): [Playlist_mutation_response]

  """
  update data of the table: "Track"
  """
  update_Track(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Track_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Track_set_input

    """filter the rows which have to be updated"""
    where: Track_bool_exp!
  ): Track_mutation_response

  """
  update single row of the table: "Track"
  """
  update_Track_by_pk(
    """increments the numeric columns with given value of the filtered values"""
    _inc: Track_inc_input

    """sets the columns of the filtered rows to the given values"""
    _set: Track_set_input
    pk_columns: Track_pk_columns_input!
  ): Track

  """
  update multiples rows of table: "Track"
  """
  update_Track_many(
    """updates to execute, in order"""
    updates: [Track_updates!]!
  ): [Track_mutation_response]
}

scalar numeric

"""
Boolean expression to compare columns of type "numeric". All fields are combined with logical 'AND'.
"""
input numeric_comparison_exp {
  _eq: numeric
  _gt: numeric
  _gte: numeric
  _in: [numeric!]
  _is_null: Boolean
  _lt: numeric
  _lte: numeric
  _neq: numeric
  _nin: [numeric!]
}

"""column ordering options"""
enum order_by {
  """in ascending order, nulls last"""
  asc

  """in ascending order, nulls first"""
  asc_nulls_first

  """in ascending order, nulls last"""
  asc_nulls_last

  """in descending order, nulls first"""
  desc

  """in descending order, nulls first"""
  desc_nulls_first

  """in descending order, nulls last"""
  desc_nulls_last
}

type query_root {
  """
  fetch data from the table: "Album"
  """
  Album(
    """distinct select on columns"""
    distinct_on: [Album_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Album_order_by!]

    """filter the rows returned"""
    where: Album_bool_exp
  ): [Album!]!

  """
  fetch aggregated fields from the table: "Album"
  """
  Album_aggregate(
    """distinct select on columns"""
    distinct_on: [Album_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Album_order_by!]

    """filter the rows returned"""
    where: Album_bool_exp
  ): Album_aggregate!

  """fetch data from the table: "Album" using primary key columns"""
  Album_by_pk(AlbumId: Int!): Album

  """
  fetch data from the table: "Artist"
  """
  Artist(
    """distinct select on columns"""
    distinct_on: [Artist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Artist_order_by!]

    """filter the rows returned"""
    where: Artist_bool_exp
  ): [Artist!]!

  """
  fetch aggregated fields from the table: "Artist"
  """
  Artist_aggregate(
    """distinct select on columns"""
    distinct_on: [Artist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Artist_order_by!]

    """filter the rows returned"""
    where: Artist_bool_exp
  ): Artist_aggregate!

  """fetch data from the table: "Artist" using primary key columns"""
  Artist_by_pk(ArtistId: Int!): Artist

  """
  fetch data from the table: "Customer"
  """
  Customer(
    """distinct select on columns"""
    distinct_on: [Customer_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Customer_order_by!]

    """filter the rows returned"""
    where: Customer_bool_exp
  ): [Customer!]!

  """
  fetch aggregated fields from the table: "Customer"
  """
  Customer_aggregate(
    """distinct select on columns"""
    distinct_on: [Customer_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Customer_order_by!]

    """filter the rows returned"""
    where: Customer_bool_exp
  ): Customer_aggregate!

  """fetch data from the table: "Customer" using primary key columns"""
  Customer_by_pk(CustomerId: Int!): Customer

  """
  fetch data from the table: "Employee"
  """
  Employee(
    """distinct select on columns"""
    distinct_on: [Employee_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Employee_order_by!]

    """filter the rows returned"""
    where: Employee_bool_exp
  ): [Employee!]!

  """
  fetch aggregated fields from the table: "Employee"
  """
  Employee_aggregate(
    """distinct select on columns"""
    distinct_on: [Employee_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Employee_order_by!]

    """filter the rows returned"""
    where: Employee_bool_exp
  ): Employee_aggregate!

  """fetch data from the table: "Employee" using primary key columns"""
  Employee_by_pk(EmployeeId: Int!): Employee

  """
  fetch data from the table: "Genre"
  """
  Genre(
    """distinct select on columns"""
    distinct_on: [Genre_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Genre_order_by!]

    """filter the rows returned"""
    where: Genre_bool_exp
  ): [Genre!]!

  """
  fetch aggregated fields from the table: "Genre"
  """
  Genre_aggregate(
    """distinct select on columns"""
    distinct_on: [Genre_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Genre_order_by!]

    """filter the rows returned"""
    where: Genre_bool_exp
  ): Genre_aggregate!

  """fetch data from the table: "Genre" using primary key columns"""
  Genre_by_pk(GenreId: Int!): Genre

  """
  fetch data from the table: "Invoice"
  """
  Invoice(
    """distinct select on columns"""
    distinct_on: [Invoice_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Invoice_order_by!]

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): [Invoice!]!

  """
  fetch data from the table: "InvoiceLine"
  """
  InvoiceLine(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): [InvoiceLine!]!

  """
  fetch aggregated fields from the table: "InvoiceLine"
  """
  InvoiceLine_aggregate(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): InvoiceLine_aggregate!

  """fetch data from the table: "InvoiceLine" using primary key columns"""
  InvoiceLine_by_pk(InvoiceLineId: Int!): InvoiceLine

  """
  fetch aggregated fields from the table: "Invoice"
  """
  Invoice_aggregate(
    """distinct select on columns"""
    distinct_on: [Invoice_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Invoice_order_by!]

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): Invoice_aggregate!

  """fetch data from the table: "Invoice" using primary key columns"""
  Invoice_by_pk(InvoiceId: Int!): Invoice

  """
  fetch data from the table: "MediaType"
  """
  MediaType(
    """distinct select on columns"""
    distinct_on: [MediaType_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [MediaType_order_by!]

    """filter the rows returned"""
    where: MediaType_bool_exp
  ): [MediaType!]!

  """
  fetch aggregated fields from the table: "MediaType"
  """
  MediaType_aggregate(
    """distinct select on columns"""
    distinct_on: [MediaType_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [MediaType_order_by!]

    """filter the rows returned"""
    where: MediaType_bool_exp
  ): MediaType_aggregate!

  """fetch data from the table: "MediaType" using primary key columns"""
  MediaType_by_pk(MediaTypeId: Int!): MediaType

  """
  fetch data from the table: "Playlist"
  """
  Playlist(
    """distinct select on columns"""
    distinct_on: [Playlist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Playlist_order_by!]

    """filter the rows returned"""
    where: Playlist_bool_exp
  ): [Playlist!]!

  """
  fetch data from the table: "PlaylistTrack"
  """
  PlaylistTrack(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): [PlaylistTrack!]!

  """
  fetch aggregated fields from the table: "PlaylistTrack"
  """
  PlaylistTrack_aggregate(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): PlaylistTrack_aggregate!

  """fetch data from the table: "PlaylistTrack" using primary key columns"""
  PlaylistTrack_by_pk(PlaylistId: Int!, TrackId: Int!): PlaylistTrack

  """
  fetch aggregated fields from the table: "Playlist"
  """
  Playlist_aggregate(
    """distinct select on columns"""
    distinct_on: [Playlist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Playlist_order_by!]

    """filter the rows returned"""
    where: Playlist_bool_exp
  ): Playlist_aggregate!

  """fetch data from the table: "Playlist" using primary key columns"""
  Playlist_by_pk(PlaylistId: Int!): Playlist

  """
  fetch data from the table: "Track"
  """
  Track(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): [Track!]!

  """
  fetch aggregated fields from the table: "Track"
  """
  Track_aggregate(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): Track_aggregate!

  """fetch data from the table: "Track" using primary key columns"""
  Track_by_pk(TrackId: Int!): Track
}

type subscription_root {
  """
  fetch data from the table: "Album"
  """
  Album(
    """distinct select on columns"""
    distinct_on: [Album_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Album_order_by!]

    """filter the rows returned"""
    where: Album_bool_exp
  ): [Album!]!

  """
  fetch aggregated fields from the table: "Album"
  """
  Album_aggregate(
    """distinct select on columns"""
    distinct_on: [Album_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Album_order_by!]

    """filter the rows returned"""
    where: Album_bool_exp
  ): Album_aggregate!

  """fetch data from the table: "Album" using primary key columns"""
  Album_by_pk(AlbumId: Int!): Album

  """
  fetch data from the table in a streaming manner: "Album"
  """
  Album_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Album_stream_cursor_input]!

    """filter the rows returned"""
    where: Album_bool_exp
  ): [Album!]!

  """
  fetch data from the table: "Artist"
  """
  Artist(
    """distinct select on columns"""
    distinct_on: [Artist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Artist_order_by!]

    """filter the rows returned"""
    where: Artist_bool_exp
  ): [Artist!]!

  """
  fetch aggregated fields from the table: "Artist"
  """
  Artist_aggregate(
    """distinct select on columns"""
    distinct_on: [Artist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Artist_order_by!]

    """filter the rows returned"""
    where: Artist_bool_exp
  ): Artist_aggregate!

  """fetch data from the table: "Artist" using primary key columns"""
  Artist_by_pk(ArtistId: Int!): Artist

  """
  fetch data from the table in a streaming manner: "Artist"
  """
  Artist_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Artist_stream_cursor_input]!

    """filter the rows returned"""
    where: Artist_bool_exp
  ): [Artist!]!

  """
  fetch data from the table: "Customer"
  """
  Customer(
    """distinct select on columns"""
    distinct_on: [Customer_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Customer_order_by!]

    """filter the rows returned"""
    where: Customer_bool_exp
  ): [Customer!]!

  """
  fetch aggregated fields from the table: "Customer"
  """
  Customer_aggregate(
    """distinct select on columns"""
    distinct_on: [Customer_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Customer_order_by!]

    """filter the rows returned"""
    where: Customer_bool_exp
  ): Customer_aggregate!

  """fetch data from the table: "Customer" using primary key columns"""
  Customer_by_pk(CustomerId: Int!): Customer

  """
  fetch data from the table in a streaming manner: "Customer"
  """
  Customer_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Customer_stream_cursor_input]!

    """filter the rows returned"""
    where: Customer_bool_exp
  ): [Customer!]!

  """
  fetch data from the table: "Employee"
  """
  Employee(
    """distinct select on columns"""
    distinct_on: [Employee_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Employee_order_by!]

    """filter the rows returned"""
    where: Employee_bool_exp
  ): [Employee!]!

  """
  fetch aggregated fields from the table: "Employee"
  """
  Employee_aggregate(
    """distinct select on columns"""
    distinct_on: [Employee_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Employee_order_by!]

    """filter the rows returned"""
    where: Employee_bool_exp
  ): Employee_aggregate!

  """fetch data from the table: "Employee" using primary key columns"""
  Employee_by_pk(EmployeeId: Int!): Employee

  """
  fetch data from the table in a streaming manner: "Employee"
  """
  Employee_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Employee_stream_cursor_input]!

    """filter the rows returned"""
    where: Employee_bool_exp
  ): [Employee!]!

  """
  fetch data from the table: "Genre"
  """
  Genre(
    """distinct select on columns"""
    distinct_on: [Genre_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Genre_order_by!]

    """filter the rows returned"""
    where: Genre_bool_exp
  ): [Genre!]!

  """
  fetch aggregated fields from the table: "Genre"
  """
  Genre_aggregate(
    """distinct select on columns"""
    distinct_on: [Genre_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Genre_order_by!]

    """filter the rows returned"""
    where: Genre_bool_exp
  ): Genre_aggregate!

  """fetch data from the table: "Genre" using primary key columns"""
  Genre_by_pk(GenreId: Int!): Genre

  """
  fetch data from the table in a streaming manner: "Genre"
  """
  Genre_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Genre_stream_cursor_input]!

    """filter the rows returned"""
    where: Genre_bool_exp
  ): [Genre!]!

  """
  fetch data from the table: "Invoice"
  """
  Invoice(
    """distinct select on columns"""
    distinct_on: [Invoice_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Invoice_order_by!]

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): [Invoice!]!

  """
  fetch data from the table: "InvoiceLine"
  """
  InvoiceLine(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): [InvoiceLine!]!

  """
  fetch aggregated fields from the table: "InvoiceLine"
  """
  InvoiceLine_aggregate(
    """distinct select on columns"""
    distinct_on: [InvoiceLine_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [InvoiceLine_order_by!]

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): InvoiceLine_aggregate!

  """fetch data from the table: "InvoiceLine" using primary key columns"""
  InvoiceLine_by_pk(InvoiceLineId: Int!): InvoiceLine

  """
  fetch data from the table in a streaming manner: "InvoiceLine"
  """
  InvoiceLine_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [InvoiceLine_stream_cursor_input]!

    """filter the rows returned"""
    where: InvoiceLine_bool_exp
  ): [InvoiceLine!]!

  """
  fetch aggregated fields from the table: "Invoice"
  """
  Invoice_aggregate(
    """distinct select on columns"""
    distinct_on: [Invoice_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Invoice_order_by!]

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): Invoice_aggregate!

  """fetch data from the table: "Invoice" using primary key columns"""
  Invoice_by_pk(InvoiceId: Int!): Invoice

  """
  fetch data from the table in a streaming manner: "Invoice"
  """
  Invoice_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Invoice_stream_cursor_input]!

    """filter the rows returned"""
    where: Invoice_bool_exp
  ): [Invoice!]!

  """
  fetch data from the table: "MediaType"
  """
  MediaType(
    """distinct select on columns"""
    distinct_on: [MediaType_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [MediaType_order_by!]

    """filter the rows returned"""
    where: MediaType_bool_exp
  ): [MediaType!]!

  """
  fetch aggregated fields from the table: "MediaType"
  """
  MediaType_aggregate(
    """distinct select on columns"""
    distinct_on: [MediaType_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [MediaType_order_by!]

    """filter the rows returned"""
    where: MediaType_bool_exp
  ): MediaType_aggregate!

  """fetch data from the table: "MediaType" using primary key columns"""
  MediaType_by_pk(MediaTypeId: Int!): MediaType

  """
  fetch data from the table in a streaming manner: "MediaType"
  """
  MediaType_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [MediaType_stream_cursor_input]!

    """filter the rows returned"""
    where: MediaType_bool_exp
  ): [MediaType!]!

  """
  fetch data from the table: "Playlist"
  """
  Playlist(
    """distinct select on columns"""
    distinct_on: [Playlist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Playlist_order_by!]

    """filter the rows returned"""
    where: Playlist_bool_exp
  ): [Playlist!]!

  """
  fetch data from the table: "PlaylistTrack"
  """
  PlaylistTrack(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): [PlaylistTrack!]!

  """
  fetch aggregated fields from the table: "PlaylistTrack"
  """
  PlaylistTrack_aggregate(
    """distinct select on columns"""
    distinct_on: [PlaylistTrack_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [PlaylistTrack_order_by!]

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): PlaylistTrack_aggregate!

  """fetch data from the table: "PlaylistTrack" using primary key columns"""
  PlaylistTrack_by_pk(PlaylistId: Int!, TrackId: Int!): PlaylistTrack

  """
  fetch data from the table in a streaming manner: "PlaylistTrack"
  """
  PlaylistTrack_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [PlaylistTrack_stream_cursor_input]!

    """filter the rows returned"""
    where: PlaylistTrack_bool_exp
  ): [PlaylistTrack!]!

  """
  fetch aggregated fields from the table: "Playlist"
  """
  Playlist_aggregate(
    """distinct select on columns"""
    distinct_on: [Playlist_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Playlist_order_by!]

    """filter the rows returned"""
    where: Playlist_bool_exp
  ): Playlist_aggregate!

  """fetch data from the table: "Playlist" using primary key columns"""
  Playlist_by_pk(PlaylistId: Int!): Playlist

  """
  fetch data from the table in a streaming manner: "Playlist"
  """
  Playlist_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Playlist_stream_cursor_input]!

    """filter the rows returned"""
    where: Playlist_bool_exp
  ): [Playlist!]!

  """
  fetch data from the table: "Track"
  """
  Track(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): [Track!]!

  """
  fetch aggregated fields from the table: "Track"
  """
  Track_aggregate(
    """distinct select on columns"""
    distinct_on: [Track_select_column!]

    """limit the number of rows returned"""
    limit: Int

    """skip the first n rows. Use only with order_by"""
    offset: Int

    """sort the rows by one or more columns"""
    order_by: [Track_order_by!]

    """filter the rows returned"""
    where: Track_bool_exp
  ): Track_aggregate!

  """fetch data from the table: "Track" using primary key columns"""
  Track_by_pk(TrackId: Int!): Track

  """
  fetch data from the table in a streaming manner: "Track"
  """
  Track_stream(
    """maximum number of rows returned in a single batch"""
    batch_size: Int!

    """cursor to stream the results returned by the query"""
    cursor: [Track_stream_cursor_input]!

    """filter the rows returned"""
    where: Track_bool_exp
  ): [Track!]!
}

scalar timestamp

"""
Boolean expression to compare columns of type "timestamp". All fields are combined with logical 'AND'.
"""
input timestamp_comparison_exp {
  _eq: timestamp
  _gt: timestamp
  _gte: timestamp
  _in: [timestamp!]
  _is_null: Boolean
  _lt: timestamp
  _lte: timestamp
  _neq: timestamp
  _nin: [timestamp!]
}
`;
