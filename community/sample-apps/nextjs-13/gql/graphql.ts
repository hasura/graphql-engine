/* eslint-disable */
import { TypedDocumentNode as DocumentNode } from '@graphql-typed-document-node/core';
export type Maybe<T> = T | null;
export type InputMaybe<T> = Maybe<T>;
export type Exact<T extends { [key: string]: unknown }> = { [K in keyof T]: T[K] };
export type MakeOptional<T, K extends keyof T> = Omit<T, K> & { [SubKey in K]?: Maybe<T[SubKey]> };
export type MakeMaybe<T, K extends keyof T> = Omit<T, K> & { [SubKey in K]: Maybe<T[SubKey]> };
/** All built-in and custom scalars, mapped to their actual values */
export type Scalars = {
  ID: string;
  String: string;
  Boolean: boolean;
  Int: number;
  Float: number;
  jsonb: any;
  numeric: number;
  timestamptz: any;
};

export type AdminLoginInput = {
  email: Scalars['String'];
  password: Scalars['String'];
};

export type AdminSignupInput = {
  email: Scalars['String'];
  name: Scalars['String'];
  password: Scalars['String'];
};

/** Boolean expression to compare columns of type "Boolean". All fields are combined with logical 'AND'. */
export type Boolean_Comparison_Exp = {
  _eq?: InputMaybe<Scalars['Boolean']>;
  _gt?: InputMaybe<Scalars['Boolean']>;
  _gte?: InputMaybe<Scalars['Boolean']>;
  _in?: InputMaybe<Array<Scalars['Boolean']>>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  _lt?: InputMaybe<Scalars['Boolean']>;
  _lte?: InputMaybe<Scalars['Boolean']>;
  _neq?: InputMaybe<Scalars['Boolean']>;
  _nin?: InputMaybe<Array<Scalars['Boolean']>>;
};

export type CreatePaymentIntentInput = {
  paymentAmount: Scalars['Float'];
};

/** Boolean expression to compare columns of type "Int". All fields are combined with logical 'AND'. */
export type Int_Comparison_Exp = {
  _eq?: InputMaybe<Scalars['Int']>;
  _gt?: InputMaybe<Scalars['Int']>;
  _gte?: InputMaybe<Scalars['Int']>;
  _in?: InputMaybe<Array<Scalars['Int']>>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  _lt?: InputMaybe<Scalars['Int']>;
  _lte?: InputMaybe<Scalars['Int']>;
  _neq?: InputMaybe<Scalars['Int']>;
  _nin?: InputMaybe<Array<Scalars['Int']>>;
};

export type Jwt = {
  email: Scalars['String'];
  name: Scalars['String'];
  refreshToken: Scalars['String'];
  token: Scalars['String'];
};

export type LoginInput = {
  email: Scalars['String'];
  password: Scalars['String'];
};

export type PaymentIntentClientSecret = {
  clientSecret: Scalars['String'];
};

export type RefreshTokenInput = {
  refreshToken: Scalars['String'];
};

export type RefreshTokenJwt = {
  token: Scalars['String'];
};

export type SignupInput = {
  email: Scalars['String'];
  name: Scalars['String'];
  password: Scalars['String'];
};

/** Boolean expression to compare columns of type "String". All fields are combined with logical 'AND'. */
export type String_Comparison_Exp = {
  _eq?: InputMaybe<Scalars['String']>;
  _gt?: InputMaybe<Scalars['String']>;
  _gte?: InputMaybe<Scalars['String']>;
  /** does the column match the given case-insensitive pattern */
  _ilike?: InputMaybe<Scalars['String']>;
  _in?: InputMaybe<Array<Scalars['String']>>;
  /** does the column match the given POSIX regular expression, case insensitive */
  _iregex?: InputMaybe<Scalars['String']>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  /** does the column match the given pattern */
  _like?: InputMaybe<Scalars['String']>;
  _lt?: InputMaybe<Scalars['String']>;
  _lte?: InputMaybe<Scalars['String']>;
  _neq?: InputMaybe<Scalars['String']>;
  /** does the column NOT match the given case-insensitive pattern */
  _nilike?: InputMaybe<Scalars['String']>;
  _nin?: InputMaybe<Array<Scalars['String']>>;
  /** does the column NOT match the given POSIX regular expression, case insensitive */
  _niregex?: InputMaybe<Scalars['String']>;
  /** does the column NOT match the given pattern */
  _nlike?: InputMaybe<Scalars['String']>;
  /** does the column NOT match the given POSIX regular expression, case sensitive */
  _nregex?: InputMaybe<Scalars['String']>;
  /** does the column NOT match the given SQL regular expression */
  _nsimilar?: InputMaybe<Scalars['String']>;
  /** does the column match the given POSIX regular expression, case sensitive */
  _regex?: InputMaybe<Scalars['String']>;
  /** does the column match the given SQL regular expression */
  _similar?: InputMaybe<Scalars['String']>;
};

/** A physical billing/shipping address, attached to a user account */
export type Address = {
  address_line_one: Scalars['String'];
  address_line_two?: Maybe<Scalars['String']>;
  city: Scalars['String'];
  created_at: Scalars['timestamptz'];
  id: Scalars['Int'];
  name?: Maybe<Scalars['String']>;
  /** An array relationship */
  orders_with_billing_address: Array<Order>;
  /** An aggregate relationship */
  orders_with_billing_address_aggregate: Order_Aggregate;
  /** An array relationship */
  orders_with_shipping_address: Array<Order>;
  /** An aggregate relationship */
  orders_with_shipping_address_aggregate: Order_Aggregate;
  state: Scalars['String'];
  updated_at: Scalars['timestamptz'];
  /** An object relationship */
  user: User;
  user_id: Scalars['Int'];
  zipcode: Scalars['String'];
};


/** A physical billing/shipping address, attached to a user account */
export type AddressOrders_With_Billing_AddressArgs = {
  distinct_on?: InputMaybe<Array<Order_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Order_By>>;
  where?: InputMaybe<Order_Bool_Exp>;
};


/** A physical billing/shipping address, attached to a user account */
export type AddressOrders_With_Billing_Address_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Order_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Order_By>>;
  where?: InputMaybe<Order_Bool_Exp>;
};


/** A physical billing/shipping address, attached to a user account */
export type AddressOrders_With_Shipping_AddressArgs = {
  distinct_on?: InputMaybe<Array<Order_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Order_By>>;
  where?: InputMaybe<Order_Bool_Exp>;
};


/** A physical billing/shipping address, attached to a user account */
export type AddressOrders_With_Shipping_Address_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Order_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Order_By>>;
  where?: InputMaybe<Order_Bool_Exp>;
};

/** aggregated selection of "address" */
export type Address_Aggregate = {
  aggregate?: Maybe<Address_Aggregate_Fields>;
  nodes: Array<Address>;
};

export type Address_Aggregate_Bool_Exp = {
  count?: InputMaybe<Address_Aggregate_Bool_Exp_Count>;
};

export type Address_Aggregate_Bool_Exp_Count = {
  arguments?: InputMaybe<Array<Address_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
  filter?: InputMaybe<Address_Bool_Exp>;
  predicate: Int_Comparison_Exp;
};

/** aggregate fields of "address" */
export type Address_Aggregate_Fields = {
  avg?: Maybe<Address_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Address_Max_Fields>;
  min?: Maybe<Address_Min_Fields>;
  stddev?: Maybe<Address_Stddev_Fields>;
  stddev_pop?: Maybe<Address_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Address_Stddev_Samp_Fields>;
  sum?: Maybe<Address_Sum_Fields>;
  var_pop?: Maybe<Address_Var_Pop_Fields>;
  var_samp?: Maybe<Address_Var_Samp_Fields>;
  variance?: Maybe<Address_Variance_Fields>;
};


/** aggregate fields of "address" */
export type Address_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Address_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "address" */
export type Address_Aggregate_Order_By = {
  avg?: InputMaybe<Address_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Address_Max_Order_By>;
  min?: InputMaybe<Address_Min_Order_By>;
  stddev?: InputMaybe<Address_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Address_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Address_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Address_Sum_Order_By>;
  var_pop?: InputMaybe<Address_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Address_Var_Samp_Order_By>;
  variance?: InputMaybe<Address_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "address" */
export type Address_Arr_Rel_Insert_Input = {
  data: Array<Address_Insert_Input>;
  /** upsert condition */
  on_conflict?: InputMaybe<Address_On_Conflict>;
};

/** aggregate avg on columns */
export type Address_Avg_Fields = {
  id?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "address" */
export type Address_Avg_Order_By = {
  id?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "address". All fields are combined with a logical 'AND'. */
export type Address_Bool_Exp = {
  _and?: InputMaybe<Array<Address_Bool_Exp>>;
  _not?: InputMaybe<Address_Bool_Exp>;
  _or?: InputMaybe<Array<Address_Bool_Exp>>;
  address_line_one?: InputMaybe<String_Comparison_Exp>;
  address_line_two?: InputMaybe<String_Comparison_Exp>;
  city?: InputMaybe<String_Comparison_Exp>;
  created_at?: InputMaybe<Timestamptz_Comparison_Exp>;
  id?: InputMaybe<Int_Comparison_Exp>;
  name?: InputMaybe<String_Comparison_Exp>;
  orders_with_billing_address?: InputMaybe<Order_Bool_Exp>;
  orders_with_billing_address_aggregate?: InputMaybe<Order_Aggregate_Bool_Exp>;
  orders_with_shipping_address?: InputMaybe<Order_Bool_Exp>;
  orders_with_shipping_address_aggregate?: InputMaybe<Order_Aggregate_Bool_Exp>;
  state?: InputMaybe<String_Comparison_Exp>;
  updated_at?: InputMaybe<Timestamptz_Comparison_Exp>;
  user?: InputMaybe<User_Bool_Exp>;
  user_id?: InputMaybe<Int_Comparison_Exp>;
  zipcode?: InputMaybe<String_Comparison_Exp>;
};

/** unique or primary key constraints on table "address" */
export type Address_Constraint =
  /** unique or primary key constraint on columns "id" */
  | 'address_pkey';

/** input type for incrementing numeric columns in table "address" */
export type Address_Inc_Input = {
  id?: InputMaybe<Scalars['Int']>;
  user_id?: InputMaybe<Scalars['Int']>;
};

/** input type for inserting data into table "address" */
export type Address_Insert_Input = {
  address_line_one?: InputMaybe<Scalars['String']>;
  address_line_two?: InputMaybe<Scalars['String']>;
  city?: InputMaybe<Scalars['String']>;
  created_at?: InputMaybe<Scalars['timestamptz']>;
  id?: InputMaybe<Scalars['Int']>;
  name?: InputMaybe<Scalars['String']>;
  orders_with_billing_address?: InputMaybe<Order_Arr_Rel_Insert_Input>;
  orders_with_shipping_address?: InputMaybe<Order_Arr_Rel_Insert_Input>;
  state?: InputMaybe<Scalars['String']>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
  user?: InputMaybe<User_Obj_Rel_Insert_Input>;
  user_id?: InputMaybe<Scalars['Int']>;
  zipcode?: InputMaybe<Scalars['String']>;
};

/** aggregate max on columns */
export type Address_Max_Fields = {
  address_line_one?: Maybe<Scalars['String']>;
  address_line_two?: Maybe<Scalars['String']>;
  city?: Maybe<Scalars['String']>;
  created_at?: Maybe<Scalars['timestamptz']>;
  id?: Maybe<Scalars['Int']>;
  name?: Maybe<Scalars['String']>;
  state?: Maybe<Scalars['String']>;
  updated_at?: Maybe<Scalars['timestamptz']>;
  user_id?: Maybe<Scalars['Int']>;
  zipcode?: Maybe<Scalars['String']>;
};

/** order by max() on columns of table "address" */
export type Address_Max_Order_By = {
  address_line_one?: InputMaybe<Order_By>;
  address_line_two?: InputMaybe<Order_By>;
  city?: InputMaybe<Order_By>;
  created_at?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  name?: InputMaybe<Order_By>;
  state?: InputMaybe<Order_By>;
  updated_at?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
  zipcode?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Address_Min_Fields = {
  address_line_one?: Maybe<Scalars['String']>;
  address_line_two?: Maybe<Scalars['String']>;
  city?: Maybe<Scalars['String']>;
  created_at?: Maybe<Scalars['timestamptz']>;
  id?: Maybe<Scalars['Int']>;
  name?: Maybe<Scalars['String']>;
  state?: Maybe<Scalars['String']>;
  updated_at?: Maybe<Scalars['timestamptz']>;
  user_id?: Maybe<Scalars['Int']>;
  zipcode?: Maybe<Scalars['String']>;
};

/** order by min() on columns of table "address" */
export type Address_Min_Order_By = {
  address_line_one?: InputMaybe<Order_By>;
  address_line_two?: InputMaybe<Order_By>;
  city?: InputMaybe<Order_By>;
  created_at?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  name?: InputMaybe<Order_By>;
  state?: InputMaybe<Order_By>;
  updated_at?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
  zipcode?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "address" */
export type Address_Mutation_Response = {
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Address>;
};

/** input type for inserting object relation for remote table "address" */
export type Address_Obj_Rel_Insert_Input = {
  data: Address_Insert_Input;
  /** upsert condition */
  on_conflict?: InputMaybe<Address_On_Conflict>;
};

/** on_conflict condition type for table "address" */
export type Address_On_Conflict = {
  constraint: Address_Constraint;
  update_columns?: Array<Address_Update_Column>;
  where?: InputMaybe<Address_Bool_Exp>;
};

/** Ordering options when selecting data from "address". */
export type Address_Order_By = {
  address_line_one?: InputMaybe<Order_By>;
  address_line_two?: InputMaybe<Order_By>;
  city?: InputMaybe<Order_By>;
  created_at?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  name?: InputMaybe<Order_By>;
  orders_with_billing_address_aggregate?: InputMaybe<Order_Aggregate_Order_By>;
  orders_with_shipping_address_aggregate?: InputMaybe<Order_Aggregate_Order_By>;
  state?: InputMaybe<Order_By>;
  updated_at?: InputMaybe<Order_By>;
  user?: InputMaybe<User_Order_By>;
  user_id?: InputMaybe<Order_By>;
  zipcode?: InputMaybe<Order_By>;
};

/** primary key columns input for table: address */
export type Address_Pk_Columns_Input = {
  id: Scalars['Int'];
};

/** select columns of table "address" */
export type Address_Select_Column =
  /** column name */
  | 'address_line_one'
  /** column name */
  | 'address_line_two'
  /** column name */
  | 'city'
  /** column name */
  | 'created_at'
  /** column name */
  | 'id'
  /** column name */
  | 'name'
  /** column name */
  | 'state'
  /** column name */
  | 'updated_at'
  /** column name */
  | 'user_id'
  /** column name */
  | 'zipcode';

/** input type for updating data in table "address" */
export type Address_Set_Input = {
  address_line_one?: InputMaybe<Scalars['String']>;
  address_line_two?: InputMaybe<Scalars['String']>;
  city?: InputMaybe<Scalars['String']>;
  created_at?: InputMaybe<Scalars['timestamptz']>;
  id?: InputMaybe<Scalars['Int']>;
  name?: InputMaybe<Scalars['String']>;
  state?: InputMaybe<Scalars['String']>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
  user_id?: InputMaybe<Scalars['Int']>;
  zipcode?: InputMaybe<Scalars['String']>;
};

/** aggregate stddev on columns */
export type Address_Stddev_Fields = {
  id?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "address" */
export type Address_Stddev_Order_By = {
  id?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Address_Stddev_Pop_Fields = {
  id?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "address" */
export type Address_Stddev_Pop_Order_By = {
  id?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Address_Stddev_Samp_Fields = {
  id?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "address" */
export type Address_Stddev_Samp_Order_By = {
  id?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** Streaming cursor of the table "address" */
export type Address_Stream_Cursor_Input = {
  /** Stream column input with initial value */
  initial_value: Address_Stream_Cursor_Value_Input;
  /** cursor ordering */
  ordering?: InputMaybe<Cursor_Ordering>;
};

/** Initial value of the column from where the streaming should start */
export type Address_Stream_Cursor_Value_Input = {
  address_line_one?: InputMaybe<Scalars['String']>;
  address_line_two?: InputMaybe<Scalars['String']>;
  city?: InputMaybe<Scalars['String']>;
  created_at?: InputMaybe<Scalars['timestamptz']>;
  id?: InputMaybe<Scalars['Int']>;
  name?: InputMaybe<Scalars['String']>;
  state?: InputMaybe<Scalars['String']>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
  user_id?: InputMaybe<Scalars['Int']>;
  zipcode?: InputMaybe<Scalars['String']>;
};

/** aggregate sum on columns */
export type Address_Sum_Fields = {
  id?: Maybe<Scalars['Int']>;
  user_id?: Maybe<Scalars['Int']>;
};

/** order by sum() on columns of table "address" */
export type Address_Sum_Order_By = {
  id?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** update columns of table "address" */
export type Address_Update_Column =
  /** column name */
  | 'address_line_one'
  /** column name */
  | 'address_line_two'
  /** column name */
  | 'city'
  /** column name */
  | 'created_at'
  /** column name */
  | 'id'
  /** column name */
  | 'name'
  /** column name */
  | 'state'
  /** column name */
  | 'updated_at'
  /** column name */
  | 'user_id'
  /** column name */
  | 'zipcode';

export type Address_Updates = {
  /** increments the numeric columns with given value of the filtered values */
  _inc?: InputMaybe<Address_Inc_Input>;
  /** sets the columns of the filtered rows to the given values */
  _set?: InputMaybe<Address_Set_Input>;
  where: Address_Bool_Exp;
};

/** aggregate var_pop on columns */
export type Address_Var_Pop_Fields = {
  id?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "address" */
export type Address_Var_Pop_Order_By = {
  id?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Address_Var_Samp_Fields = {
  id?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "address" */
export type Address_Var_Samp_Order_By = {
  id?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Address_Variance_Fields = {
  id?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "address" */
export type Address_Variance_Order_By = {
  id?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** ordering argument of a cursor */
export type Cursor_Ordering =
  /** ascending ordering of the cursor */
  | 'ASC'
  /** descending ordering of the cursor */
  | 'DESC';

export type Jsonb_Cast_Exp = {
  String?: InputMaybe<String_Comparison_Exp>;
};

/** Boolean expression to compare columns of type "jsonb". All fields are combined with logical 'AND'. */
export type Jsonb_Comparison_Exp = {
  _cast?: InputMaybe<Jsonb_Cast_Exp>;
  /** is the column contained in the given json value */
  _contained_in?: InputMaybe<Scalars['jsonb']>;
  /** does the column contain the given json value at the top level */
  _contains?: InputMaybe<Scalars['jsonb']>;
  _eq?: InputMaybe<Scalars['jsonb']>;
  _gt?: InputMaybe<Scalars['jsonb']>;
  _gte?: InputMaybe<Scalars['jsonb']>;
  /** does the string exist as a top-level key in the column */
  _has_key?: InputMaybe<Scalars['String']>;
  /** do all of these strings exist as top-level keys in the column */
  _has_keys_all?: InputMaybe<Array<Scalars['String']>>;
  /** do any of these strings exist as top-level keys in the column */
  _has_keys_any?: InputMaybe<Array<Scalars['String']>>;
  _in?: InputMaybe<Array<Scalars['jsonb']>>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  _lt?: InputMaybe<Scalars['jsonb']>;
  _lte?: InputMaybe<Scalars['jsonb']>;
  _neq?: InputMaybe<Scalars['jsonb']>;
  _nin?: InputMaybe<Array<Scalars['jsonb']>>;
};

/** mutation root */
export type Mutation_Root = {
  adminSignup?: Maybe<Jwt>;
  createPaymentIntent?: Maybe<PaymentIntentClientSecret>;
  /** delete data from the table: "address" */
  delete_address?: Maybe<Address_Mutation_Response>;
  /** delete single row from the table: "address" */
  delete_address_by_pk?: Maybe<Address>;
  /** delete data from the table: "order" */
  delete_order?: Maybe<Order_Mutation_Response>;
  /** delete single row from the table: "order" */
  delete_order_by_pk?: Maybe<Order>;
  /** delete data from the table: "order_product" */
  delete_order_product?: Maybe<Order_Product_Mutation_Response>;
  /** delete single row from the table: "order_product" */
  delete_order_product_by_pk?: Maybe<Order_Product>;
  /** delete data from the table: "order_status" */
  delete_order_status?: Maybe<Order_Status_Mutation_Response>;
  /** delete single row from the table: "order_status" */
  delete_order_status_by_pk?: Maybe<Order_Status>;
  /** delete data from the table: "product" */
  delete_product?: Maybe<Product_Mutation_Response>;
  /** delete single row from the table: "product" */
  delete_product_by_pk?: Maybe<Product>;
  /** delete data from the table: "product_category_enum" */
  delete_product_category_enum?: Maybe<Product_Category_Enum_Mutation_Response>;
  /** delete single row from the table: "product_category_enum" */
  delete_product_category_enum_by_pk?: Maybe<Product_Category_Enum>;
  /** delete data from the table: "product_review" */
  delete_product_review?: Maybe<Product_Review_Mutation_Response>;
  /** delete single row from the table: "product_review" */
  delete_product_review_by_pk?: Maybe<Product_Review>;
  /** delete data from the table: "site_admin" */
  delete_site_admin?: Maybe<Site_Admin_Mutation_Response>;
  /** delete single row from the table: "site_admin" */
  delete_site_admin_by_pk?: Maybe<Site_Admin>;
  /** delete data from the table: "user" */
  delete_user?: Maybe<User_Mutation_Response>;
  /** delete single row from the table: "user" */
  delete_user_by_pk?: Maybe<User>;
  /** insert data into the table: "address" */
  insert_address?: Maybe<Address_Mutation_Response>;
  /** insert a single row into the table: "address" */
  insert_address_one?: Maybe<Address>;
  /** insert data into the table: "order" */
  insert_order?: Maybe<Order_Mutation_Response>;
  /** insert a single row into the table: "order" */
  insert_order_one?: Maybe<Order>;
  /** insert data into the table: "order_product" */
  insert_order_product?: Maybe<Order_Product_Mutation_Response>;
  /** insert a single row into the table: "order_product" */
  insert_order_product_one?: Maybe<Order_Product>;
  /** insert data into the table: "order_status" */
  insert_order_status?: Maybe<Order_Status_Mutation_Response>;
  /** insert a single row into the table: "order_status" */
  insert_order_status_one?: Maybe<Order_Status>;
  /** insert data into the table: "product" */
  insert_product?: Maybe<Product_Mutation_Response>;
  /** insert data into the table: "product_category_enum" */
  insert_product_category_enum?: Maybe<Product_Category_Enum_Mutation_Response>;
  /** insert a single row into the table: "product_category_enum" */
  insert_product_category_enum_one?: Maybe<Product_Category_Enum>;
  /** insert a single row into the table: "product" */
  insert_product_one?: Maybe<Product>;
  /** insert data into the table: "product_review" */
  insert_product_review?: Maybe<Product_Review_Mutation_Response>;
  /** insert a single row into the table: "product_review" */
  insert_product_review_one?: Maybe<Product_Review>;
  /** insert data into the table: "site_admin" */
  insert_site_admin?: Maybe<Site_Admin_Mutation_Response>;
  /** insert a single row into the table: "site_admin" */
  insert_site_admin_one?: Maybe<Site_Admin>;
  /** insert data into the table: "user" */
  insert_user?: Maybe<User_Mutation_Response>;
  /** insert a single row into the table: "user" */
  insert_user_one?: Maybe<User>;
  login?: Maybe<Jwt>;
  signup?: Maybe<Jwt>;
  /** update data of the table: "address" */
  update_address?: Maybe<Address_Mutation_Response>;
  /** update single row of the table: "address" */
  update_address_by_pk?: Maybe<Address>;
  /** update multiples rows of table: "address" */
  update_address_many?: Maybe<Array<Maybe<Address_Mutation_Response>>>;
  /** update data of the table: "order" */
  update_order?: Maybe<Order_Mutation_Response>;
  /** update single row of the table: "order" */
  update_order_by_pk?: Maybe<Order>;
  /** update multiples rows of table: "order" */
  update_order_many?: Maybe<Array<Maybe<Order_Mutation_Response>>>;
  /** update data of the table: "order_product" */
  update_order_product?: Maybe<Order_Product_Mutation_Response>;
  /** update single row of the table: "order_product" */
  update_order_product_by_pk?: Maybe<Order_Product>;
  /** update multiples rows of table: "order_product" */
  update_order_product_many?: Maybe<Array<Maybe<Order_Product_Mutation_Response>>>;
  /** update data of the table: "order_status" */
  update_order_status?: Maybe<Order_Status_Mutation_Response>;
  /** update single row of the table: "order_status" */
  update_order_status_by_pk?: Maybe<Order_Status>;
  /** update multiples rows of table: "order_status" */
  update_order_status_many?: Maybe<Array<Maybe<Order_Status_Mutation_Response>>>;
  /** update data of the table: "product" */
  update_product?: Maybe<Product_Mutation_Response>;
  /** update single row of the table: "product" */
  update_product_by_pk?: Maybe<Product>;
  /** update data of the table: "product_category_enum" */
  update_product_category_enum?: Maybe<Product_Category_Enum_Mutation_Response>;
  /** update single row of the table: "product_category_enum" */
  update_product_category_enum_by_pk?: Maybe<Product_Category_Enum>;
  /** update multiples rows of table: "product_category_enum" */
  update_product_category_enum_many?: Maybe<Array<Maybe<Product_Category_Enum_Mutation_Response>>>;
  /** update multiples rows of table: "product" */
  update_product_many?: Maybe<Array<Maybe<Product_Mutation_Response>>>;
  /** update data of the table: "product_review" */
  update_product_review?: Maybe<Product_Review_Mutation_Response>;
  /** update single row of the table: "product_review" */
  update_product_review_by_pk?: Maybe<Product_Review>;
  /** update multiples rows of table: "product_review" */
  update_product_review_many?: Maybe<Array<Maybe<Product_Review_Mutation_Response>>>;
  /** update data of the table: "site_admin" */
  update_site_admin?: Maybe<Site_Admin_Mutation_Response>;
  /** update single row of the table: "site_admin" */
  update_site_admin_by_pk?: Maybe<Site_Admin>;
  /** update multiples rows of table: "site_admin" */
  update_site_admin_many?: Maybe<Array<Maybe<Site_Admin_Mutation_Response>>>;
  /** update data of the table: "user" */
  update_user?: Maybe<User_Mutation_Response>;
  /** update single row of the table: "user" */
  update_user_by_pk?: Maybe<User>;
  /** update multiples rows of table: "user" */
  update_user_many?: Maybe<Array<Maybe<User_Mutation_Response>>>;
};


/** mutation root */
export type Mutation_RootAdminSignupArgs = {
  params: AdminSignupInput;
};


/** mutation root */
export type Mutation_RootCreatePaymentIntentArgs = {
  params: CreatePaymentIntentInput;
};


/** mutation root */
export type Mutation_RootDelete_AddressArgs = {
  where: Address_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Address_By_PkArgs = {
  id: Scalars['Int'];
};


/** mutation root */
export type Mutation_RootDelete_OrderArgs = {
  where: Order_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Order_By_PkArgs = {
  id: Scalars['Int'];
};


/** mutation root */
export type Mutation_RootDelete_Order_ProductArgs = {
  where: Order_Product_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Order_Product_By_PkArgs = {
  id: Scalars['Int'];
};


/** mutation root */
export type Mutation_RootDelete_Order_StatusArgs = {
  where: Order_Status_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Order_Status_By_PkArgs = {
  status: Scalars['String'];
};


/** mutation root */
export type Mutation_RootDelete_ProductArgs = {
  where: Product_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Product_By_PkArgs = {
  id: Scalars['Int'];
};


/** mutation root */
export type Mutation_RootDelete_Product_Category_EnumArgs = {
  where: Product_Category_Enum_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Product_Category_Enum_By_PkArgs = {
  name: Scalars['String'];
};


/** mutation root */
export type Mutation_RootDelete_Product_ReviewArgs = {
  where: Product_Review_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Product_Review_By_PkArgs = {
  id: Scalars['Int'];
};


/** mutation root */
export type Mutation_RootDelete_Site_AdminArgs = {
  where: Site_Admin_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Site_Admin_By_PkArgs = {
  id: Scalars['Int'];
};


/** mutation root */
export type Mutation_RootDelete_UserArgs = {
  where: User_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_User_By_PkArgs = {
  id: Scalars['Int'];
};


/** mutation root */
export type Mutation_RootInsert_AddressArgs = {
  objects: Array<Address_Insert_Input>;
  on_conflict?: InputMaybe<Address_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Address_OneArgs = {
  object: Address_Insert_Input;
  on_conflict?: InputMaybe<Address_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_OrderArgs = {
  objects: Array<Order_Insert_Input>;
  on_conflict?: InputMaybe<Order_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Order_OneArgs = {
  object: Order_Insert_Input;
  on_conflict?: InputMaybe<Order_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Order_ProductArgs = {
  objects: Array<Order_Product_Insert_Input>;
  on_conflict?: InputMaybe<Order_Product_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Order_Product_OneArgs = {
  object: Order_Product_Insert_Input;
  on_conflict?: InputMaybe<Order_Product_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Order_StatusArgs = {
  objects: Array<Order_Status_Insert_Input>;
  on_conflict?: InputMaybe<Order_Status_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Order_Status_OneArgs = {
  object: Order_Status_Insert_Input;
  on_conflict?: InputMaybe<Order_Status_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_ProductArgs = {
  objects: Array<Product_Insert_Input>;
  on_conflict?: InputMaybe<Product_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Product_Category_EnumArgs = {
  objects: Array<Product_Category_Enum_Insert_Input>;
  on_conflict?: InputMaybe<Product_Category_Enum_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Product_Category_Enum_OneArgs = {
  object: Product_Category_Enum_Insert_Input;
  on_conflict?: InputMaybe<Product_Category_Enum_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Product_OneArgs = {
  object: Product_Insert_Input;
  on_conflict?: InputMaybe<Product_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Product_ReviewArgs = {
  objects: Array<Product_Review_Insert_Input>;
  on_conflict?: InputMaybe<Product_Review_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Product_Review_OneArgs = {
  object: Product_Review_Insert_Input;
  on_conflict?: InputMaybe<Product_Review_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Site_AdminArgs = {
  objects: Array<Site_Admin_Insert_Input>;
  on_conflict?: InputMaybe<Site_Admin_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Site_Admin_OneArgs = {
  object: Site_Admin_Insert_Input;
  on_conflict?: InputMaybe<Site_Admin_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_UserArgs = {
  objects: Array<User_Insert_Input>;
  on_conflict?: InputMaybe<User_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_User_OneArgs = {
  object: User_Insert_Input;
  on_conflict?: InputMaybe<User_On_Conflict>;
};


/** mutation root */
export type Mutation_RootLoginArgs = {
  params: LoginInput;
};


/** mutation root */
export type Mutation_RootSignupArgs = {
  params: SignupInput;
};


/** mutation root */
export type Mutation_RootUpdate_AddressArgs = {
  _inc?: InputMaybe<Address_Inc_Input>;
  _set?: InputMaybe<Address_Set_Input>;
  where: Address_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Address_By_PkArgs = {
  _inc?: InputMaybe<Address_Inc_Input>;
  _set?: InputMaybe<Address_Set_Input>;
  pk_columns: Address_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Address_ManyArgs = {
  updates: Array<Address_Updates>;
};


/** mutation root */
export type Mutation_RootUpdate_OrderArgs = {
  _inc?: InputMaybe<Order_Inc_Input>;
  _set?: InputMaybe<Order_Set_Input>;
  where: Order_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Order_By_PkArgs = {
  _inc?: InputMaybe<Order_Inc_Input>;
  _set?: InputMaybe<Order_Set_Input>;
  pk_columns: Order_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Order_ManyArgs = {
  updates: Array<Order_Updates>;
};


/** mutation root */
export type Mutation_RootUpdate_Order_ProductArgs = {
  _inc?: InputMaybe<Order_Product_Inc_Input>;
  _set?: InputMaybe<Order_Product_Set_Input>;
  where: Order_Product_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Order_Product_By_PkArgs = {
  _inc?: InputMaybe<Order_Product_Inc_Input>;
  _set?: InputMaybe<Order_Product_Set_Input>;
  pk_columns: Order_Product_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Order_Product_ManyArgs = {
  updates: Array<Order_Product_Updates>;
};


/** mutation root */
export type Mutation_RootUpdate_Order_StatusArgs = {
  _set?: InputMaybe<Order_Status_Set_Input>;
  where: Order_Status_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Order_Status_By_PkArgs = {
  _set?: InputMaybe<Order_Status_Set_Input>;
  pk_columns: Order_Status_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Order_Status_ManyArgs = {
  updates: Array<Order_Status_Updates>;
};


/** mutation root */
export type Mutation_RootUpdate_ProductArgs = {
  _append?: InputMaybe<Product_Append_Input>;
  _delete_at_path?: InputMaybe<Product_Delete_At_Path_Input>;
  _delete_elem?: InputMaybe<Product_Delete_Elem_Input>;
  _delete_key?: InputMaybe<Product_Delete_Key_Input>;
  _inc?: InputMaybe<Product_Inc_Input>;
  _prepend?: InputMaybe<Product_Prepend_Input>;
  _set?: InputMaybe<Product_Set_Input>;
  where: Product_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Product_By_PkArgs = {
  _append?: InputMaybe<Product_Append_Input>;
  _delete_at_path?: InputMaybe<Product_Delete_At_Path_Input>;
  _delete_elem?: InputMaybe<Product_Delete_Elem_Input>;
  _delete_key?: InputMaybe<Product_Delete_Key_Input>;
  _inc?: InputMaybe<Product_Inc_Input>;
  _prepend?: InputMaybe<Product_Prepend_Input>;
  _set?: InputMaybe<Product_Set_Input>;
  pk_columns: Product_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Product_Category_EnumArgs = {
  _set?: InputMaybe<Product_Category_Enum_Set_Input>;
  where: Product_Category_Enum_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Product_Category_Enum_By_PkArgs = {
  _set?: InputMaybe<Product_Category_Enum_Set_Input>;
  pk_columns: Product_Category_Enum_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Product_Category_Enum_ManyArgs = {
  updates: Array<Product_Category_Enum_Updates>;
};


/** mutation root */
export type Mutation_RootUpdate_Product_ManyArgs = {
  updates: Array<Product_Updates>;
};


/** mutation root */
export type Mutation_RootUpdate_Product_ReviewArgs = {
  _inc?: InputMaybe<Product_Review_Inc_Input>;
  _set?: InputMaybe<Product_Review_Set_Input>;
  where: Product_Review_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Product_Review_By_PkArgs = {
  _inc?: InputMaybe<Product_Review_Inc_Input>;
  _set?: InputMaybe<Product_Review_Set_Input>;
  pk_columns: Product_Review_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Product_Review_ManyArgs = {
  updates: Array<Product_Review_Updates>;
};


/** mutation root */
export type Mutation_RootUpdate_Site_AdminArgs = {
  _inc?: InputMaybe<Site_Admin_Inc_Input>;
  _set?: InputMaybe<Site_Admin_Set_Input>;
  where: Site_Admin_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Site_Admin_By_PkArgs = {
  _inc?: InputMaybe<Site_Admin_Inc_Input>;
  _set?: InputMaybe<Site_Admin_Set_Input>;
  pk_columns: Site_Admin_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Site_Admin_ManyArgs = {
  updates: Array<Site_Admin_Updates>;
};


/** mutation root */
export type Mutation_RootUpdate_UserArgs = {
  _inc?: InputMaybe<User_Inc_Input>;
  _set?: InputMaybe<User_Set_Input>;
  where: User_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_User_By_PkArgs = {
  _inc?: InputMaybe<User_Inc_Input>;
  _set?: InputMaybe<User_Set_Input>;
  pk_columns: User_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_User_ManyArgs = {
  updates: Array<User_Updates>;
};

/** Boolean expression to compare columns of type "numeric". All fields are combined with logical 'AND'. */
export type Numeric_Comparison_Exp = {
  _eq?: InputMaybe<Scalars['numeric']>;
  _gt?: InputMaybe<Scalars['numeric']>;
  _gte?: InputMaybe<Scalars['numeric']>;
  _in?: InputMaybe<Array<Scalars['numeric']>>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  _lt?: InputMaybe<Scalars['numeric']>;
  _lte?: InputMaybe<Scalars['numeric']>;
  _neq?: InputMaybe<Scalars['numeric']>;
  _nin?: InputMaybe<Array<Scalars['numeric']>>;
};

/** An order from a customer, containing one or more products and quantities */
export type Order = {
  /** An object relationship */
  billing_address: Address;
  billing_address_id: Scalars['Int'];
  created_at: Scalars['timestamptz'];
  id: Scalars['Int'];
  is_shipped: Scalars['Boolean'];
  /** An object relationship */
  order_status: Order_Status;
  order_total?: Maybe<Scalars['numeric']>;
  /** An array relationship */
  products: Array<Order_Product>;
  /** An aggregate relationship */
  products_aggregate: Order_Product_Aggregate;
  /** An object relationship */
  shipping_address: Address;
  shipping_address_id: Scalars['Int'];
  status: Order_Status_Enum;
  updated_at: Scalars['timestamptz'];
  /** An object relationship */
  user: User;
  user_id: Scalars['Int'];
};


/** An order from a customer, containing one or more products and quantities */
export type OrderProductsArgs = {
  distinct_on?: InputMaybe<Array<Order_Product_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Product_Order_By>>;
  where?: InputMaybe<Order_Product_Bool_Exp>;
};


/** An order from a customer, containing one or more products and quantities */
export type OrderProducts_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Order_Product_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Product_Order_By>>;
  where?: InputMaybe<Order_Product_Bool_Exp>;
};

/** aggregated selection of "order" */
export type Order_Aggregate = {
  aggregate?: Maybe<Order_Aggregate_Fields>;
  nodes: Array<Order>;
};

export type Order_Aggregate_Bool_Exp = {
  bool_and?: InputMaybe<Order_Aggregate_Bool_Exp_Bool_And>;
  bool_or?: InputMaybe<Order_Aggregate_Bool_Exp_Bool_Or>;
  count?: InputMaybe<Order_Aggregate_Bool_Exp_Count>;
};

export type Order_Aggregate_Bool_Exp_Bool_And = {
  arguments: Order_Select_Column_Order_Aggregate_Bool_Exp_Bool_And_Arguments_Columns;
  distinct?: InputMaybe<Scalars['Boolean']>;
  filter?: InputMaybe<Order_Bool_Exp>;
  predicate: Boolean_Comparison_Exp;
};

export type Order_Aggregate_Bool_Exp_Bool_Or = {
  arguments: Order_Select_Column_Order_Aggregate_Bool_Exp_Bool_Or_Arguments_Columns;
  distinct?: InputMaybe<Scalars['Boolean']>;
  filter?: InputMaybe<Order_Bool_Exp>;
  predicate: Boolean_Comparison_Exp;
};

export type Order_Aggregate_Bool_Exp_Count = {
  arguments?: InputMaybe<Array<Order_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
  filter?: InputMaybe<Order_Bool_Exp>;
  predicate: Int_Comparison_Exp;
};

/** aggregate fields of "order" */
export type Order_Aggregate_Fields = {
  avg?: Maybe<Order_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Order_Max_Fields>;
  min?: Maybe<Order_Min_Fields>;
  stddev?: Maybe<Order_Stddev_Fields>;
  stddev_pop?: Maybe<Order_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Order_Stddev_Samp_Fields>;
  sum?: Maybe<Order_Sum_Fields>;
  var_pop?: Maybe<Order_Var_Pop_Fields>;
  var_samp?: Maybe<Order_Var_Samp_Fields>;
  variance?: Maybe<Order_Variance_Fields>;
};


/** aggregate fields of "order" */
export type Order_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Order_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "order" */
export type Order_Aggregate_Order_By = {
  avg?: InputMaybe<Order_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Order_Max_Order_By>;
  min?: InputMaybe<Order_Min_Order_By>;
  stddev?: InputMaybe<Order_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Order_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Order_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Order_Sum_Order_By>;
  var_pop?: InputMaybe<Order_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Order_Var_Samp_Order_By>;
  variance?: InputMaybe<Order_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "order" */
export type Order_Arr_Rel_Insert_Input = {
  data: Array<Order_Insert_Input>;
  /** upsert condition */
  on_conflict?: InputMaybe<Order_On_Conflict>;
};

/** aggregate avg on columns */
export type Order_Avg_Fields = {
  billing_address_id?: Maybe<Scalars['Float']>;
  id?: Maybe<Scalars['Float']>;
  order_total?: Maybe<Scalars['Float']>;
  shipping_address_id?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "order" */
export type Order_Avg_Order_By = {
  billing_address_id?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  order_total?: InputMaybe<Order_By>;
  shipping_address_id?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "order". All fields are combined with a logical 'AND'. */
export type Order_Bool_Exp = {
  _and?: InputMaybe<Array<Order_Bool_Exp>>;
  _not?: InputMaybe<Order_Bool_Exp>;
  _or?: InputMaybe<Array<Order_Bool_Exp>>;
  billing_address?: InputMaybe<Address_Bool_Exp>;
  billing_address_id?: InputMaybe<Int_Comparison_Exp>;
  created_at?: InputMaybe<Timestamptz_Comparison_Exp>;
  id?: InputMaybe<Int_Comparison_Exp>;
  is_shipped?: InputMaybe<Boolean_Comparison_Exp>;
  order_status?: InputMaybe<Order_Status_Bool_Exp>;
  order_total?: InputMaybe<Numeric_Comparison_Exp>;
  products?: InputMaybe<Order_Product_Bool_Exp>;
  products_aggregate?: InputMaybe<Order_Product_Aggregate_Bool_Exp>;
  shipping_address?: InputMaybe<Address_Bool_Exp>;
  shipping_address_id?: InputMaybe<Int_Comparison_Exp>;
  status?: InputMaybe<Order_Status_Enum_Comparison_Exp>;
  updated_at?: InputMaybe<Timestamptz_Comparison_Exp>;
  user?: InputMaybe<User_Bool_Exp>;
  user_id?: InputMaybe<Int_Comparison_Exp>;
};

/** column ordering options */
export type Order_By =
  /** in ascending order, nulls last */
  | 'asc'
  /** in ascending order, nulls first */
  | 'asc_nulls_first'
  /** in ascending order, nulls last */
  | 'asc_nulls_last'
  /** in descending order, nulls first */
  | 'desc'
  /** in descending order, nulls first */
  | 'desc_nulls_first'
  /** in descending order, nulls last */
  | 'desc_nulls_last';

/** unique or primary key constraints on table "order" */
export type Order_Constraint =
  /** unique or primary key constraint on columns "id" */
  | 'order_pkey';

/** input type for incrementing numeric columns in table "order" */
export type Order_Inc_Input = {
  billing_address_id?: InputMaybe<Scalars['Int']>;
  id?: InputMaybe<Scalars['Int']>;
  order_total?: InputMaybe<Scalars['numeric']>;
  shipping_address_id?: InputMaybe<Scalars['Int']>;
  user_id?: InputMaybe<Scalars['Int']>;
};

/** input type for inserting data into table "order" */
export type Order_Insert_Input = {
  billing_address?: InputMaybe<Address_Obj_Rel_Insert_Input>;
  billing_address_id?: InputMaybe<Scalars['Int']>;
  created_at?: InputMaybe<Scalars['timestamptz']>;
  id?: InputMaybe<Scalars['Int']>;
  is_shipped?: InputMaybe<Scalars['Boolean']>;
  order_status?: InputMaybe<Order_Status_Obj_Rel_Insert_Input>;
  order_total?: InputMaybe<Scalars['numeric']>;
  products?: InputMaybe<Order_Product_Arr_Rel_Insert_Input>;
  shipping_address?: InputMaybe<Address_Obj_Rel_Insert_Input>;
  shipping_address_id?: InputMaybe<Scalars['Int']>;
  status?: InputMaybe<Order_Status_Enum>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
  user?: InputMaybe<User_Obj_Rel_Insert_Input>;
  user_id?: InputMaybe<Scalars['Int']>;
};

/** aggregate max on columns */
export type Order_Max_Fields = {
  billing_address_id?: Maybe<Scalars['Int']>;
  created_at?: Maybe<Scalars['timestamptz']>;
  id?: Maybe<Scalars['Int']>;
  order_total?: Maybe<Scalars['numeric']>;
  shipping_address_id?: Maybe<Scalars['Int']>;
  updated_at?: Maybe<Scalars['timestamptz']>;
  user_id?: Maybe<Scalars['Int']>;
};

/** order by max() on columns of table "order" */
export type Order_Max_Order_By = {
  billing_address_id?: InputMaybe<Order_By>;
  created_at?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  order_total?: InputMaybe<Order_By>;
  shipping_address_id?: InputMaybe<Order_By>;
  updated_at?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Order_Min_Fields = {
  billing_address_id?: Maybe<Scalars['Int']>;
  created_at?: Maybe<Scalars['timestamptz']>;
  id?: Maybe<Scalars['Int']>;
  order_total?: Maybe<Scalars['numeric']>;
  shipping_address_id?: Maybe<Scalars['Int']>;
  updated_at?: Maybe<Scalars['timestamptz']>;
  user_id?: Maybe<Scalars['Int']>;
};

/** order by min() on columns of table "order" */
export type Order_Min_Order_By = {
  billing_address_id?: InputMaybe<Order_By>;
  created_at?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  order_total?: InputMaybe<Order_By>;
  shipping_address_id?: InputMaybe<Order_By>;
  updated_at?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "order" */
export type Order_Mutation_Response = {
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Order>;
};

/** input type for inserting object relation for remote table "order" */
export type Order_Obj_Rel_Insert_Input = {
  data: Order_Insert_Input;
  /** upsert condition */
  on_conflict?: InputMaybe<Order_On_Conflict>;
};

/** on_conflict condition type for table "order" */
export type Order_On_Conflict = {
  constraint: Order_Constraint;
  update_columns?: Array<Order_Update_Column>;
  where?: InputMaybe<Order_Bool_Exp>;
};

/** Ordering options when selecting data from "order". */
export type Order_Order_By = {
  billing_address?: InputMaybe<Address_Order_By>;
  billing_address_id?: InputMaybe<Order_By>;
  created_at?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  is_shipped?: InputMaybe<Order_By>;
  order_status?: InputMaybe<Order_Status_Order_By>;
  order_total?: InputMaybe<Order_By>;
  products_aggregate?: InputMaybe<Order_Product_Aggregate_Order_By>;
  shipping_address?: InputMaybe<Address_Order_By>;
  shipping_address_id?: InputMaybe<Order_By>;
  status?: InputMaybe<Order_By>;
  updated_at?: InputMaybe<Order_By>;
  user?: InputMaybe<User_Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** primary key columns input for table: order */
export type Order_Pk_Columns_Input = {
  id: Scalars['Int'];
};

/** A product belonging to a customer order, along with a quantity */
export type Order_Product = {
  created_at: Scalars['timestamptz'];
  id: Scalars['Int'];
  /** An object relationship */
  order: Order;
  order_id: Scalars['Int'];
  /** An object relationship */
  product: Product;
  product_id: Scalars['Int'];
  quantity: Scalars['Int'];
  updated_at: Scalars['timestamptz'];
};

/** aggregated selection of "order_product" */
export type Order_Product_Aggregate = {
  aggregate?: Maybe<Order_Product_Aggregate_Fields>;
  nodes: Array<Order_Product>;
};

export type Order_Product_Aggregate_Bool_Exp = {
  count?: InputMaybe<Order_Product_Aggregate_Bool_Exp_Count>;
};

export type Order_Product_Aggregate_Bool_Exp_Count = {
  arguments?: InputMaybe<Array<Order_Product_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
  filter?: InputMaybe<Order_Product_Bool_Exp>;
  predicate: Int_Comparison_Exp;
};

/** aggregate fields of "order_product" */
export type Order_Product_Aggregate_Fields = {
  avg?: Maybe<Order_Product_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Order_Product_Max_Fields>;
  min?: Maybe<Order_Product_Min_Fields>;
  stddev?: Maybe<Order_Product_Stddev_Fields>;
  stddev_pop?: Maybe<Order_Product_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Order_Product_Stddev_Samp_Fields>;
  sum?: Maybe<Order_Product_Sum_Fields>;
  var_pop?: Maybe<Order_Product_Var_Pop_Fields>;
  var_samp?: Maybe<Order_Product_Var_Samp_Fields>;
  variance?: Maybe<Order_Product_Variance_Fields>;
};


/** aggregate fields of "order_product" */
export type Order_Product_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Order_Product_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "order_product" */
export type Order_Product_Aggregate_Order_By = {
  avg?: InputMaybe<Order_Product_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Order_Product_Max_Order_By>;
  min?: InputMaybe<Order_Product_Min_Order_By>;
  stddev?: InputMaybe<Order_Product_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Order_Product_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Order_Product_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Order_Product_Sum_Order_By>;
  var_pop?: InputMaybe<Order_Product_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Order_Product_Var_Samp_Order_By>;
  variance?: InputMaybe<Order_Product_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "order_product" */
export type Order_Product_Arr_Rel_Insert_Input = {
  data: Array<Order_Product_Insert_Input>;
  /** upsert condition */
  on_conflict?: InputMaybe<Order_Product_On_Conflict>;
};

/** aggregate avg on columns */
export type Order_Product_Avg_Fields = {
  id?: Maybe<Scalars['Float']>;
  order_id?: Maybe<Scalars['Float']>;
  product_id?: Maybe<Scalars['Float']>;
  quantity?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "order_product" */
export type Order_Product_Avg_Order_By = {
  id?: InputMaybe<Order_By>;
  order_id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  quantity?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "order_product". All fields are combined with a logical 'AND'. */
export type Order_Product_Bool_Exp = {
  _and?: InputMaybe<Array<Order_Product_Bool_Exp>>;
  _not?: InputMaybe<Order_Product_Bool_Exp>;
  _or?: InputMaybe<Array<Order_Product_Bool_Exp>>;
  created_at?: InputMaybe<Timestamptz_Comparison_Exp>;
  id?: InputMaybe<Int_Comparison_Exp>;
  order?: InputMaybe<Order_Bool_Exp>;
  order_id?: InputMaybe<Int_Comparison_Exp>;
  product?: InputMaybe<Product_Bool_Exp>;
  product_id?: InputMaybe<Int_Comparison_Exp>;
  quantity?: InputMaybe<Int_Comparison_Exp>;
  updated_at?: InputMaybe<Timestamptz_Comparison_Exp>;
};

/** unique or primary key constraints on table "order_product" */
export type Order_Product_Constraint =
  /** unique or primary key constraint on columns "id" */
  | 'order_product_pkey';

/** input type for incrementing numeric columns in table "order_product" */
export type Order_Product_Inc_Input = {
  id?: InputMaybe<Scalars['Int']>;
  order_id?: InputMaybe<Scalars['Int']>;
  product_id?: InputMaybe<Scalars['Int']>;
  quantity?: InputMaybe<Scalars['Int']>;
};

/** input type for inserting data into table "order_product" */
export type Order_Product_Insert_Input = {
  created_at?: InputMaybe<Scalars['timestamptz']>;
  id?: InputMaybe<Scalars['Int']>;
  order?: InputMaybe<Order_Obj_Rel_Insert_Input>;
  order_id?: InputMaybe<Scalars['Int']>;
  product?: InputMaybe<Product_Obj_Rel_Insert_Input>;
  product_id?: InputMaybe<Scalars['Int']>;
  quantity?: InputMaybe<Scalars['Int']>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate max on columns */
export type Order_Product_Max_Fields = {
  created_at?: Maybe<Scalars['timestamptz']>;
  id?: Maybe<Scalars['Int']>;
  order_id?: Maybe<Scalars['Int']>;
  product_id?: Maybe<Scalars['Int']>;
  quantity?: Maybe<Scalars['Int']>;
  updated_at?: Maybe<Scalars['timestamptz']>;
};

/** order by max() on columns of table "order_product" */
export type Order_Product_Max_Order_By = {
  created_at?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  order_id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  quantity?: InputMaybe<Order_By>;
  updated_at?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Order_Product_Min_Fields = {
  created_at?: Maybe<Scalars['timestamptz']>;
  id?: Maybe<Scalars['Int']>;
  order_id?: Maybe<Scalars['Int']>;
  product_id?: Maybe<Scalars['Int']>;
  quantity?: Maybe<Scalars['Int']>;
  updated_at?: Maybe<Scalars['timestamptz']>;
};

/** order by min() on columns of table "order_product" */
export type Order_Product_Min_Order_By = {
  created_at?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  order_id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  quantity?: InputMaybe<Order_By>;
  updated_at?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "order_product" */
export type Order_Product_Mutation_Response = {
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Order_Product>;
};

/** on_conflict condition type for table "order_product" */
export type Order_Product_On_Conflict = {
  constraint: Order_Product_Constraint;
  update_columns?: Array<Order_Product_Update_Column>;
  where?: InputMaybe<Order_Product_Bool_Exp>;
};

/** Ordering options when selecting data from "order_product". */
export type Order_Product_Order_By = {
  created_at?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  order?: InputMaybe<Order_Order_By>;
  order_id?: InputMaybe<Order_By>;
  product?: InputMaybe<Product_Order_By>;
  product_id?: InputMaybe<Order_By>;
  quantity?: InputMaybe<Order_By>;
  updated_at?: InputMaybe<Order_By>;
};

/** primary key columns input for table: order_product */
export type Order_Product_Pk_Columns_Input = {
  id: Scalars['Int'];
};

/** select columns of table "order_product" */
export type Order_Product_Select_Column =
  /** column name */
  | 'created_at'
  /** column name */
  | 'id'
  /** column name */
  | 'order_id'
  /** column name */
  | 'product_id'
  /** column name */
  | 'quantity'
  /** column name */
  | 'updated_at';

/** input type for updating data in table "order_product" */
export type Order_Product_Set_Input = {
  created_at?: InputMaybe<Scalars['timestamptz']>;
  id?: InputMaybe<Scalars['Int']>;
  order_id?: InputMaybe<Scalars['Int']>;
  product_id?: InputMaybe<Scalars['Int']>;
  quantity?: InputMaybe<Scalars['Int']>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate stddev on columns */
export type Order_Product_Stddev_Fields = {
  id?: Maybe<Scalars['Float']>;
  order_id?: Maybe<Scalars['Float']>;
  product_id?: Maybe<Scalars['Float']>;
  quantity?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "order_product" */
export type Order_Product_Stddev_Order_By = {
  id?: InputMaybe<Order_By>;
  order_id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  quantity?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Order_Product_Stddev_Pop_Fields = {
  id?: Maybe<Scalars['Float']>;
  order_id?: Maybe<Scalars['Float']>;
  product_id?: Maybe<Scalars['Float']>;
  quantity?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "order_product" */
export type Order_Product_Stddev_Pop_Order_By = {
  id?: InputMaybe<Order_By>;
  order_id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  quantity?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Order_Product_Stddev_Samp_Fields = {
  id?: Maybe<Scalars['Float']>;
  order_id?: Maybe<Scalars['Float']>;
  product_id?: Maybe<Scalars['Float']>;
  quantity?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "order_product" */
export type Order_Product_Stddev_Samp_Order_By = {
  id?: InputMaybe<Order_By>;
  order_id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  quantity?: InputMaybe<Order_By>;
};

/** Streaming cursor of the table "order_product" */
export type Order_Product_Stream_Cursor_Input = {
  /** Stream column input with initial value */
  initial_value: Order_Product_Stream_Cursor_Value_Input;
  /** cursor ordering */
  ordering?: InputMaybe<Cursor_Ordering>;
};

/** Initial value of the column from where the streaming should start */
export type Order_Product_Stream_Cursor_Value_Input = {
  created_at?: InputMaybe<Scalars['timestamptz']>;
  id?: InputMaybe<Scalars['Int']>;
  order_id?: InputMaybe<Scalars['Int']>;
  product_id?: InputMaybe<Scalars['Int']>;
  quantity?: InputMaybe<Scalars['Int']>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate sum on columns */
export type Order_Product_Sum_Fields = {
  id?: Maybe<Scalars['Int']>;
  order_id?: Maybe<Scalars['Int']>;
  product_id?: Maybe<Scalars['Int']>;
  quantity?: Maybe<Scalars['Int']>;
};

/** order by sum() on columns of table "order_product" */
export type Order_Product_Sum_Order_By = {
  id?: InputMaybe<Order_By>;
  order_id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  quantity?: InputMaybe<Order_By>;
};

/** update columns of table "order_product" */
export type Order_Product_Update_Column =
  /** column name */
  | 'created_at'
  /** column name */
  | 'id'
  /** column name */
  | 'order_id'
  /** column name */
  | 'product_id'
  /** column name */
  | 'quantity'
  /** column name */
  | 'updated_at';

export type Order_Product_Updates = {
  /** increments the numeric columns with given value of the filtered values */
  _inc?: InputMaybe<Order_Product_Inc_Input>;
  /** sets the columns of the filtered rows to the given values */
  _set?: InputMaybe<Order_Product_Set_Input>;
  where: Order_Product_Bool_Exp;
};

/** aggregate var_pop on columns */
export type Order_Product_Var_Pop_Fields = {
  id?: Maybe<Scalars['Float']>;
  order_id?: Maybe<Scalars['Float']>;
  product_id?: Maybe<Scalars['Float']>;
  quantity?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "order_product" */
export type Order_Product_Var_Pop_Order_By = {
  id?: InputMaybe<Order_By>;
  order_id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  quantity?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Order_Product_Var_Samp_Fields = {
  id?: Maybe<Scalars['Float']>;
  order_id?: Maybe<Scalars['Float']>;
  product_id?: Maybe<Scalars['Float']>;
  quantity?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "order_product" */
export type Order_Product_Var_Samp_Order_By = {
  id?: InputMaybe<Order_By>;
  order_id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  quantity?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Order_Product_Variance_Fields = {
  id?: Maybe<Scalars['Float']>;
  order_id?: Maybe<Scalars['Float']>;
  product_id?: Maybe<Scalars['Float']>;
  quantity?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "order_product" */
export type Order_Product_Variance_Order_By = {
  id?: InputMaybe<Order_By>;
  order_id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  quantity?: InputMaybe<Order_By>;
};

/** select columns of table "order" */
export type Order_Select_Column =
  /** column name */
  | 'billing_address_id'
  /** column name */
  | 'created_at'
  /** column name */
  | 'id'
  /** column name */
  | 'is_shipped'
  /** column name */
  | 'order_total'
  /** column name */
  | 'shipping_address_id'
  /** column name */
  | 'status'
  /** column name */
  | 'updated_at'
  /** column name */
  | 'user_id';

/** select "order_aggregate_bool_exp_bool_and_arguments_columns" columns of table "order" */
export type Order_Select_Column_Order_Aggregate_Bool_Exp_Bool_And_Arguments_Columns =
  /** column name */
  | 'is_shipped';

/** select "order_aggregate_bool_exp_bool_or_arguments_columns" columns of table "order" */
export type Order_Select_Column_Order_Aggregate_Bool_Exp_Bool_Or_Arguments_Columns =
  /** column name */
  | 'is_shipped';

/** input type for updating data in table "order" */
export type Order_Set_Input = {
  billing_address_id?: InputMaybe<Scalars['Int']>;
  created_at?: InputMaybe<Scalars['timestamptz']>;
  id?: InputMaybe<Scalars['Int']>;
  is_shipped?: InputMaybe<Scalars['Boolean']>;
  order_total?: InputMaybe<Scalars['numeric']>;
  shipping_address_id?: InputMaybe<Scalars['Int']>;
  status?: InputMaybe<Order_Status_Enum>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
  user_id?: InputMaybe<Scalars['Int']>;
};

/** columns and relationships of "order_status" */
export type Order_Status = {
  /** An array relationship */
  orders: Array<Order>;
  /** An aggregate relationship */
  orders_aggregate: Order_Aggregate;
  status: Scalars['String'];
};


/** columns and relationships of "order_status" */
export type Order_StatusOrdersArgs = {
  distinct_on?: InputMaybe<Array<Order_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Order_By>>;
  where?: InputMaybe<Order_Bool_Exp>;
};


/** columns and relationships of "order_status" */
export type Order_StatusOrders_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Order_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Order_By>>;
  where?: InputMaybe<Order_Bool_Exp>;
};

/** aggregated selection of "order_status" */
export type Order_Status_Aggregate = {
  aggregate?: Maybe<Order_Status_Aggregate_Fields>;
  nodes: Array<Order_Status>;
};

/** aggregate fields of "order_status" */
export type Order_Status_Aggregate_Fields = {
  count: Scalars['Int'];
  max?: Maybe<Order_Status_Max_Fields>;
  min?: Maybe<Order_Status_Min_Fields>;
};


/** aggregate fields of "order_status" */
export type Order_Status_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Order_Status_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** Boolean expression to filter rows from the table "order_status". All fields are combined with a logical 'AND'. */
export type Order_Status_Bool_Exp = {
  _and?: InputMaybe<Array<Order_Status_Bool_Exp>>;
  _not?: InputMaybe<Order_Status_Bool_Exp>;
  _or?: InputMaybe<Array<Order_Status_Bool_Exp>>;
  orders?: InputMaybe<Order_Bool_Exp>;
  orders_aggregate?: InputMaybe<Order_Aggregate_Bool_Exp>;
  status?: InputMaybe<String_Comparison_Exp>;
};

/** unique or primary key constraints on table "order_status" */
export type Order_Status_Constraint =
  /** unique or primary key constraint on columns "status" */
  | 'order_status_pkey';

export type Order_Status_Enum =
  | 'CANCELLED'
  | 'CREATED'
  | 'DELIVERED'
  | 'PAID'
  | 'REFUNDED'
  | 'SHIPPED';

/** Boolean expression to compare columns of type "order_status_enum". All fields are combined with logical 'AND'. */
export type Order_Status_Enum_Comparison_Exp = {
  _eq?: InputMaybe<Order_Status_Enum>;
  _in?: InputMaybe<Array<Order_Status_Enum>>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  _neq?: InputMaybe<Order_Status_Enum>;
  _nin?: InputMaybe<Array<Order_Status_Enum>>;
};

/** input type for inserting data into table "order_status" */
export type Order_Status_Insert_Input = {
  orders?: InputMaybe<Order_Arr_Rel_Insert_Input>;
  status?: InputMaybe<Scalars['String']>;
};

/** aggregate max on columns */
export type Order_Status_Max_Fields = {
  status?: Maybe<Scalars['String']>;
};

/** aggregate min on columns */
export type Order_Status_Min_Fields = {
  status?: Maybe<Scalars['String']>;
};

/** response of any mutation on the table "order_status" */
export type Order_Status_Mutation_Response = {
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Order_Status>;
};

/** input type for inserting object relation for remote table "order_status" */
export type Order_Status_Obj_Rel_Insert_Input = {
  data: Order_Status_Insert_Input;
  /** upsert condition */
  on_conflict?: InputMaybe<Order_Status_On_Conflict>;
};

/** on_conflict condition type for table "order_status" */
export type Order_Status_On_Conflict = {
  constraint: Order_Status_Constraint;
  update_columns?: Array<Order_Status_Update_Column>;
  where?: InputMaybe<Order_Status_Bool_Exp>;
};

/** Ordering options when selecting data from "order_status". */
export type Order_Status_Order_By = {
  orders_aggregate?: InputMaybe<Order_Aggregate_Order_By>;
  status?: InputMaybe<Order_By>;
};

/** primary key columns input for table: order_status */
export type Order_Status_Pk_Columns_Input = {
  status: Scalars['String'];
};

/** select columns of table "order_status" */
export type Order_Status_Select_Column =
  /** column name */
  | 'status';

/** input type for updating data in table "order_status" */
export type Order_Status_Set_Input = {
  status?: InputMaybe<Scalars['String']>;
};

/** Streaming cursor of the table "order_status" */
export type Order_Status_Stream_Cursor_Input = {
  /** Stream column input with initial value */
  initial_value: Order_Status_Stream_Cursor_Value_Input;
  /** cursor ordering */
  ordering?: InputMaybe<Cursor_Ordering>;
};

/** Initial value of the column from where the streaming should start */
export type Order_Status_Stream_Cursor_Value_Input = {
  status?: InputMaybe<Scalars['String']>;
};

/** update columns of table "order_status" */
export type Order_Status_Update_Column =
  /** column name */
  | 'status';

export type Order_Status_Updates = {
  /** sets the columns of the filtered rows to the given values */
  _set?: InputMaybe<Order_Status_Set_Input>;
  where: Order_Status_Bool_Exp;
};

/** aggregate stddev on columns */
export type Order_Stddev_Fields = {
  billing_address_id?: Maybe<Scalars['Float']>;
  id?: Maybe<Scalars['Float']>;
  order_total?: Maybe<Scalars['Float']>;
  shipping_address_id?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "order" */
export type Order_Stddev_Order_By = {
  billing_address_id?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  order_total?: InputMaybe<Order_By>;
  shipping_address_id?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Order_Stddev_Pop_Fields = {
  billing_address_id?: Maybe<Scalars['Float']>;
  id?: Maybe<Scalars['Float']>;
  order_total?: Maybe<Scalars['Float']>;
  shipping_address_id?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "order" */
export type Order_Stddev_Pop_Order_By = {
  billing_address_id?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  order_total?: InputMaybe<Order_By>;
  shipping_address_id?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Order_Stddev_Samp_Fields = {
  billing_address_id?: Maybe<Scalars['Float']>;
  id?: Maybe<Scalars['Float']>;
  order_total?: Maybe<Scalars['Float']>;
  shipping_address_id?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "order" */
export type Order_Stddev_Samp_Order_By = {
  billing_address_id?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  order_total?: InputMaybe<Order_By>;
  shipping_address_id?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** Streaming cursor of the table "order" */
export type Order_Stream_Cursor_Input = {
  /** Stream column input with initial value */
  initial_value: Order_Stream_Cursor_Value_Input;
  /** cursor ordering */
  ordering?: InputMaybe<Cursor_Ordering>;
};

/** Initial value of the column from where the streaming should start */
export type Order_Stream_Cursor_Value_Input = {
  billing_address_id?: InputMaybe<Scalars['Int']>;
  created_at?: InputMaybe<Scalars['timestamptz']>;
  id?: InputMaybe<Scalars['Int']>;
  is_shipped?: InputMaybe<Scalars['Boolean']>;
  order_total?: InputMaybe<Scalars['numeric']>;
  shipping_address_id?: InputMaybe<Scalars['Int']>;
  status?: InputMaybe<Order_Status_Enum>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
  user_id?: InputMaybe<Scalars['Int']>;
};

/** aggregate sum on columns */
export type Order_Sum_Fields = {
  billing_address_id?: Maybe<Scalars['Int']>;
  id?: Maybe<Scalars['Int']>;
  order_total?: Maybe<Scalars['numeric']>;
  shipping_address_id?: Maybe<Scalars['Int']>;
  user_id?: Maybe<Scalars['Int']>;
};

/** order by sum() on columns of table "order" */
export type Order_Sum_Order_By = {
  billing_address_id?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  order_total?: InputMaybe<Order_By>;
  shipping_address_id?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** update columns of table "order" */
export type Order_Update_Column =
  /** column name */
  | 'billing_address_id'
  /** column name */
  | 'created_at'
  /** column name */
  | 'id'
  /** column name */
  | 'is_shipped'
  /** column name */
  | 'order_total'
  /** column name */
  | 'shipping_address_id'
  /** column name */
  | 'status'
  /** column name */
  | 'updated_at'
  /** column name */
  | 'user_id';

export type Order_Updates = {
  /** increments the numeric columns with given value of the filtered values */
  _inc?: InputMaybe<Order_Inc_Input>;
  /** sets the columns of the filtered rows to the given values */
  _set?: InputMaybe<Order_Set_Input>;
  where: Order_Bool_Exp;
};

/** aggregate var_pop on columns */
export type Order_Var_Pop_Fields = {
  billing_address_id?: Maybe<Scalars['Float']>;
  id?: Maybe<Scalars['Float']>;
  order_total?: Maybe<Scalars['Float']>;
  shipping_address_id?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "order" */
export type Order_Var_Pop_Order_By = {
  billing_address_id?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  order_total?: InputMaybe<Order_By>;
  shipping_address_id?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Order_Var_Samp_Fields = {
  billing_address_id?: Maybe<Scalars['Float']>;
  id?: Maybe<Scalars['Float']>;
  order_total?: Maybe<Scalars['Float']>;
  shipping_address_id?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "order" */
export type Order_Var_Samp_Order_By = {
  billing_address_id?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  order_total?: InputMaybe<Order_By>;
  shipping_address_id?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Order_Variance_Fields = {
  billing_address_id?: Maybe<Scalars['Float']>;
  id?: Maybe<Scalars['Float']>;
  order_total?: Maybe<Scalars['Float']>;
  shipping_address_id?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "order" */
export type Order_Variance_Order_By = {
  billing_address_id?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  order_total?: InputMaybe<Order_By>;
  shipping_address_id?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** columns and relationships of "product" */
export type Product = {
  brand?: Maybe<Scalars['String']>;
  /** An object relationship */
  category: Product_Category_Enum;
  category_display_name: Scalars['String'];
  created_at: Scalars['timestamptz'];
  description?: Maybe<Scalars['String']>;
  id: Scalars['Int'];
  image_urls?: Maybe<Scalars['jsonb']>;
  name: Scalars['String'];
  /** An array relationship */
  orders: Array<Order_Product>;
  /** An aggregate relationship */
  orders_aggregate: Order_Product_Aggregate;
  price: Scalars['numeric'];
  /** An array relationship */
  product_reviews: Array<Product_Review>;
  /** An aggregate relationship */
  product_reviews_aggregate: Product_Review_Aggregate;
  updated_at: Scalars['timestamptz'];
};


/** columns and relationships of "product" */
export type ProductImage_UrlsArgs = {
  path?: InputMaybe<Scalars['String']>;
};


/** columns and relationships of "product" */
export type ProductOrdersArgs = {
  distinct_on?: InputMaybe<Array<Order_Product_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Product_Order_By>>;
  where?: InputMaybe<Order_Product_Bool_Exp>;
};


/** columns and relationships of "product" */
export type ProductOrders_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Order_Product_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Product_Order_By>>;
  where?: InputMaybe<Order_Product_Bool_Exp>;
};


/** columns and relationships of "product" */
export type ProductProduct_ReviewsArgs = {
  distinct_on?: InputMaybe<Array<Product_Review_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Product_Review_Order_By>>;
  where?: InputMaybe<Product_Review_Bool_Exp>;
};


/** columns and relationships of "product" */
export type ProductProduct_Reviews_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Product_Review_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Product_Review_Order_By>>;
  where?: InputMaybe<Product_Review_Bool_Exp>;
};

/** aggregated selection of "product" */
export type Product_Aggregate = {
  aggregate?: Maybe<Product_Aggregate_Fields>;
  nodes: Array<Product>;
};

export type Product_Aggregate_Bool_Exp = {
  count?: InputMaybe<Product_Aggregate_Bool_Exp_Count>;
};

export type Product_Aggregate_Bool_Exp_Count = {
  arguments?: InputMaybe<Array<Product_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
  filter?: InputMaybe<Product_Bool_Exp>;
  predicate: Int_Comparison_Exp;
};

/** aggregate fields of "product" */
export type Product_Aggregate_Fields = {
  avg?: Maybe<Product_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Product_Max_Fields>;
  min?: Maybe<Product_Min_Fields>;
  stddev?: Maybe<Product_Stddev_Fields>;
  stddev_pop?: Maybe<Product_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Product_Stddev_Samp_Fields>;
  sum?: Maybe<Product_Sum_Fields>;
  var_pop?: Maybe<Product_Var_Pop_Fields>;
  var_samp?: Maybe<Product_Var_Samp_Fields>;
  variance?: Maybe<Product_Variance_Fields>;
};


/** aggregate fields of "product" */
export type Product_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Product_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "product" */
export type Product_Aggregate_Order_By = {
  avg?: InputMaybe<Product_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Product_Max_Order_By>;
  min?: InputMaybe<Product_Min_Order_By>;
  stddev?: InputMaybe<Product_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Product_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Product_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Product_Sum_Order_By>;
  var_pop?: InputMaybe<Product_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Product_Var_Samp_Order_By>;
  variance?: InputMaybe<Product_Variance_Order_By>;
};

/** append existing jsonb value of filtered columns with new jsonb value */
export type Product_Append_Input = {
  image_urls?: InputMaybe<Scalars['jsonb']>;
};

/** input type for inserting array relation for remote table "product" */
export type Product_Arr_Rel_Insert_Input = {
  data: Array<Product_Insert_Input>;
  /** upsert condition */
  on_conflict?: InputMaybe<Product_On_Conflict>;
};

/** aggregate avg on columns */
export type Product_Avg_Fields = {
  id?: Maybe<Scalars['Float']>;
  price?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "product" */
export type Product_Avg_Order_By = {
  id?: InputMaybe<Order_By>;
  price?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "product". All fields are combined with a logical 'AND'. */
export type Product_Bool_Exp = {
  _and?: InputMaybe<Array<Product_Bool_Exp>>;
  _not?: InputMaybe<Product_Bool_Exp>;
  _or?: InputMaybe<Array<Product_Bool_Exp>>;
  brand?: InputMaybe<String_Comparison_Exp>;
  category?: InputMaybe<Product_Category_Enum_Bool_Exp>;
  category_display_name?: InputMaybe<String_Comparison_Exp>;
  created_at?: InputMaybe<Timestamptz_Comparison_Exp>;
  description?: InputMaybe<String_Comparison_Exp>;
  id?: InputMaybe<Int_Comparison_Exp>;
  image_urls?: InputMaybe<Jsonb_Comparison_Exp>;
  name?: InputMaybe<String_Comparison_Exp>;
  orders?: InputMaybe<Order_Product_Bool_Exp>;
  orders_aggregate?: InputMaybe<Order_Product_Aggregate_Bool_Exp>;
  price?: InputMaybe<Numeric_Comparison_Exp>;
  product_reviews?: InputMaybe<Product_Review_Bool_Exp>;
  product_reviews_aggregate?: InputMaybe<Product_Review_Aggregate_Bool_Exp>;
  updated_at?: InputMaybe<Timestamptz_Comparison_Exp>;
};

/** columns and relationships of "product_category_enum" */
export type Product_Category_Enum = {
  display_name: Scalars['String'];
  name: Scalars['String'];
  /** An array relationship */
  products: Array<Product>;
  /** An aggregate relationship */
  products_aggregate: Product_Aggregate;
};


/** columns and relationships of "product_category_enum" */
export type Product_Category_EnumProductsArgs = {
  distinct_on?: InputMaybe<Array<Product_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Product_Order_By>>;
  where?: InputMaybe<Product_Bool_Exp>;
};


/** columns and relationships of "product_category_enum" */
export type Product_Category_EnumProducts_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Product_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Product_Order_By>>;
  where?: InputMaybe<Product_Bool_Exp>;
};

/** aggregated selection of "product_category_enum" */
export type Product_Category_Enum_Aggregate = {
  aggregate?: Maybe<Product_Category_Enum_Aggregate_Fields>;
  nodes: Array<Product_Category_Enum>;
};

/** aggregate fields of "product_category_enum" */
export type Product_Category_Enum_Aggregate_Fields = {
  count: Scalars['Int'];
  max?: Maybe<Product_Category_Enum_Max_Fields>;
  min?: Maybe<Product_Category_Enum_Min_Fields>;
};


/** aggregate fields of "product_category_enum" */
export type Product_Category_Enum_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Product_Category_Enum_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** Boolean expression to filter rows from the table "product_category_enum". All fields are combined with a logical 'AND'. */
export type Product_Category_Enum_Bool_Exp = {
  _and?: InputMaybe<Array<Product_Category_Enum_Bool_Exp>>;
  _not?: InputMaybe<Product_Category_Enum_Bool_Exp>;
  _or?: InputMaybe<Array<Product_Category_Enum_Bool_Exp>>;
  display_name?: InputMaybe<String_Comparison_Exp>;
  name?: InputMaybe<String_Comparison_Exp>;
  products?: InputMaybe<Product_Bool_Exp>;
  products_aggregate?: InputMaybe<Product_Aggregate_Bool_Exp>;
};

/** unique or primary key constraints on table "product_category_enum" */
export type Product_Category_Enum_Constraint =
  /** unique or primary key constraint on columns "display_name" */
  | 'product_category_enum_display_name_key'
  /** unique or primary key constraint on columns "name" */
  | 'product_category_enum_pkey';

/** input type for inserting data into table "product_category_enum" */
export type Product_Category_Enum_Insert_Input = {
  display_name?: InputMaybe<Scalars['String']>;
  name?: InputMaybe<Scalars['String']>;
  products?: InputMaybe<Product_Arr_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Product_Category_Enum_Max_Fields = {
  display_name?: Maybe<Scalars['String']>;
  name?: Maybe<Scalars['String']>;
};

/** aggregate min on columns */
export type Product_Category_Enum_Min_Fields = {
  display_name?: Maybe<Scalars['String']>;
  name?: Maybe<Scalars['String']>;
};

/** response of any mutation on the table "product_category_enum" */
export type Product_Category_Enum_Mutation_Response = {
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Product_Category_Enum>;
};

/** input type for inserting object relation for remote table "product_category_enum" */
export type Product_Category_Enum_Obj_Rel_Insert_Input = {
  data: Product_Category_Enum_Insert_Input;
  /** upsert condition */
  on_conflict?: InputMaybe<Product_Category_Enum_On_Conflict>;
};

/** on_conflict condition type for table "product_category_enum" */
export type Product_Category_Enum_On_Conflict = {
  constraint: Product_Category_Enum_Constraint;
  update_columns?: Array<Product_Category_Enum_Update_Column>;
  where?: InputMaybe<Product_Category_Enum_Bool_Exp>;
};

/** Ordering options when selecting data from "product_category_enum". */
export type Product_Category_Enum_Order_By = {
  display_name?: InputMaybe<Order_By>;
  name?: InputMaybe<Order_By>;
  products_aggregate?: InputMaybe<Product_Aggregate_Order_By>;
};

/** primary key columns input for table: product_category_enum */
export type Product_Category_Enum_Pk_Columns_Input = {
  name: Scalars['String'];
};

/** select columns of table "product_category_enum" */
export type Product_Category_Enum_Select_Column =
  /** column name */
  | 'display_name'
  /** column name */
  | 'name';

/** input type for updating data in table "product_category_enum" */
export type Product_Category_Enum_Set_Input = {
  display_name?: InputMaybe<Scalars['String']>;
  name?: InputMaybe<Scalars['String']>;
};

/** Streaming cursor of the table "product_category_enum" */
export type Product_Category_Enum_Stream_Cursor_Input = {
  /** Stream column input with initial value */
  initial_value: Product_Category_Enum_Stream_Cursor_Value_Input;
  /** cursor ordering */
  ordering?: InputMaybe<Cursor_Ordering>;
};

/** Initial value of the column from where the streaming should start */
export type Product_Category_Enum_Stream_Cursor_Value_Input = {
  display_name?: InputMaybe<Scalars['String']>;
  name?: InputMaybe<Scalars['String']>;
};

/** update columns of table "product_category_enum" */
export type Product_Category_Enum_Update_Column =
  /** column name */
  | 'display_name'
  /** column name */
  | 'name';

export type Product_Category_Enum_Updates = {
  /** sets the columns of the filtered rows to the given values */
  _set?: InputMaybe<Product_Category_Enum_Set_Input>;
  where: Product_Category_Enum_Bool_Exp;
};

/** unique or primary key constraints on table "product" */
export type Product_Constraint =
  /** unique or primary key constraint on columns "id" */
  | 'product_pkey';

/** delete the field or element with specified path (for JSON arrays, negative integers count from the end) */
export type Product_Delete_At_Path_Input = {
  image_urls?: InputMaybe<Array<Scalars['String']>>;
};

/** delete the array element with specified index (negative integers count from the end). throws an error if top level container is not an array */
export type Product_Delete_Elem_Input = {
  image_urls?: InputMaybe<Scalars['Int']>;
};

/** delete key/value pair or string element. key/value pairs are matched based on their key value */
export type Product_Delete_Key_Input = {
  image_urls?: InputMaybe<Scalars['String']>;
};

/** input type for incrementing numeric columns in table "product" */
export type Product_Inc_Input = {
  id?: InputMaybe<Scalars['Int']>;
  price?: InputMaybe<Scalars['numeric']>;
};

/** input type for inserting data into table "product" */
export type Product_Insert_Input = {
  brand?: InputMaybe<Scalars['String']>;
  category?: InputMaybe<Product_Category_Enum_Obj_Rel_Insert_Input>;
  category_display_name?: InputMaybe<Scalars['String']>;
  created_at?: InputMaybe<Scalars['timestamptz']>;
  description?: InputMaybe<Scalars['String']>;
  id?: InputMaybe<Scalars['Int']>;
  image_urls?: InputMaybe<Scalars['jsonb']>;
  name?: InputMaybe<Scalars['String']>;
  orders?: InputMaybe<Order_Product_Arr_Rel_Insert_Input>;
  price?: InputMaybe<Scalars['numeric']>;
  product_reviews?: InputMaybe<Product_Review_Arr_Rel_Insert_Input>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate max on columns */
export type Product_Max_Fields = {
  brand?: Maybe<Scalars['String']>;
  category_display_name?: Maybe<Scalars['String']>;
  created_at?: Maybe<Scalars['timestamptz']>;
  description?: Maybe<Scalars['String']>;
  id?: Maybe<Scalars['Int']>;
  name?: Maybe<Scalars['String']>;
  price?: Maybe<Scalars['numeric']>;
  updated_at?: Maybe<Scalars['timestamptz']>;
};

/** order by max() on columns of table "product" */
export type Product_Max_Order_By = {
  brand?: InputMaybe<Order_By>;
  category_display_name?: InputMaybe<Order_By>;
  created_at?: InputMaybe<Order_By>;
  description?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  name?: InputMaybe<Order_By>;
  price?: InputMaybe<Order_By>;
  updated_at?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Product_Min_Fields = {
  brand?: Maybe<Scalars['String']>;
  category_display_name?: Maybe<Scalars['String']>;
  created_at?: Maybe<Scalars['timestamptz']>;
  description?: Maybe<Scalars['String']>;
  id?: Maybe<Scalars['Int']>;
  name?: Maybe<Scalars['String']>;
  price?: Maybe<Scalars['numeric']>;
  updated_at?: Maybe<Scalars['timestamptz']>;
};

/** order by min() on columns of table "product" */
export type Product_Min_Order_By = {
  brand?: InputMaybe<Order_By>;
  category_display_name?: InputMaybe<Order_By>;
  created_at?: InputMaybe<Order_By>;
  description?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  name?: InputMaybe<Order_By>;
  price?: InputMaybe<Order_By>;
  updated_at?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "product" */
export type Product_Mutation_Response = {
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Product>;
};

/** input type for inserting object relation for remote table "product" */
export type Product_Obj_Rel_Insert_Input = {
  data: Product_Insert_Input;
  /** upsert condition */
  on_conflict?: InputMaybe<Product_On_Conflict>;
};

/** on_conflict condition type for table "product" */
export type Product_On_Conflict = {
  constraint: Product_Constraint;
  update_columns?: Array<Product_Update_Column>;
  where?: InputMaybe<Product_Bool_Exp>;
};

/** Ordering options when selecting data from "product". */
export type Product_Order_By = {
  brand?: InputMaybe<Order_By>;
  category?: InputMaybe<Product_Category_Enum_Order_By>;
  category_display_name?: InputMaybe<Order_By>;
  created_at?: InputMaybe<Order_By>;
  description?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  image_urls?: InputMaybe<Order_By>;
  name?: InputMaybe<Order_By>;
  orders_aggregate?: InputMaybe<Order_Product_Aggregate_Order_By>;
  price?: InputMaybe<Order_By>;
  product_reviews_aggregate?: InputMaybe<Product_Review_Aggregate_Order_By>;
  updated_at?: InputMaybe<Order_By>;
};

/** primary key columns input for table: product */
export type Product_Pk_Columns_Input = {
  id: Scalars['Int'];
};

/** prepend existing jsonb value of filtered columns with new jsonb value */
export type Product_Prepend_Input = {
  image_urls?: InputMaybe<Scalars['jsonb']>;
};

/** A review for a product which a customer has purchased before */
export type Product_Review = {
  comment: Scalars['String'];
  created_at: Scalars['timestamptz'];
  id: Scalars['Int'];
  /** An object relationship */
  product: Product;
  product_id: Scalars['Int'];
  rating: Scalars['Int'];
  updated_at: Scalars['timestamptz'];
  /** An object relationship */
  user: User;
  user_id: Scalars['Int'];
};

/** aggregated selection of "product_review" */
export type Product_Review_Aggregate = {
  aggregate?: Maybe<Product_Review_Aggregate_Fields>;
  nodes: Array<Product_Review>;
};

export type Product_Review_Aggregate_Bool_Exp = {
  count?: InputMaybe<Product_Review_Aggregate_Bool_Exp_Count>;
};

export type Product_Review_Aggregate_Bool_Exp_Count = {
  arguments?: InputMaybe<Array<Product_Review_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
  filter?: InputMaybe<Product_Review_Bool_Exp>;
  predicate: Int_Comparison_Exp;
};

/** aggregate fields of "product_review" */
export type Product_Review_Aggregate_Fields = {
  avg?: Maybe<Product_Review_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Product_Review_Max_Fields>;
  min?: Maybe<Product_Review_Min_Fields>;
  stddev?: Maybe<Product_Review_Stddev_Fields>;
  stddev_pop?: Maybe<Product_Review_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Product_Review_Stddev_Samp_Fields>;
  sum?: Maybe<Product_Review_Sum_Fields>;
  var_pop?: Maybe<Product_Review_Var_Pop_Fields>;
  var_samp?: Maybe<Product_Review_Var_Samp_Fields>;
  variance?: Maybe<Product_Review_Variance_Fields>;
};


/** aggregate fields of "product_review" */
export type Product_Review_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Product_Review_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "product_review" */
export type Product_Review_Aggregate_Order_By = {
  avg?: InputMaybe<Product_Review_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Product_Review_Max_Order_By>;
  min?: InputMaybe<Product_Review_Min_Order_By>;
  stddev?: InputMaybe<Product_Review_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Product_Review_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Product_Review_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Product_Review_Sum_Order_By>;
  var_pop?: InputMaybe<Product_Review_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Product_Review_Var_Samp_Order_By>;
  variance?: InputMaybe<Product_Review_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "product_review" */
export type Product_Review_Arr_Rel_Insert_Input = {
  data: Array<Product_Review_Insert_Input>;
  /** upsert condition */
  on_conflict?: InputMaybe<Product_Review_On_Conflict>;
};

/** aggregate avg on columns */
export type Product_Review_Avg_Fields = {
  id?: Maybe<Scalars['Float']>;
  product_id?: Maybe<Scalars['Float']>;
  rating?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "product_review" */
export type Product_Review_Avg_Order_By = {
  id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  rating?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "product_review". All fields are combined with a logical 'AND'. */
export type Product_Review_Bool_Exp = {
  _and?: InputMaybe<Array<Product_Review_Bool_Exp>>;
  _not?: InputMaybe<Product_Review_Bool_Exp>;
  _or?: InputMaybe<Array<Product_Review_Bool_Exp>>;
  comment?: InputMaybe<String_Comparison_Exp>;
  created_at?: InputMaybe<Timestamptz_Comparison_Exp>;
  id?: InputMaybe<Int_Comparison_Exp>;
  product?: InputMaybe<Product_Bool_Exp>;
  product_id?: InputMaybe<Int_Comparison_Exp>;
  rating?: InputMaybe<Int_Comparison_Exp>;
  updated_at?: InputMaybe<Timestamptz_Comparison_Exp>;
  user?: InputMaybe<User_Bool_Exp>;
  user_id?: InputMaybe<Int_Comparison_Exp>;
};

/** unique or primary key constraints on table "product_review" */
export type Product_Review_Constraint =
  /** unique or primary key constraint on columns "user_id", "product_id" */
  | 'one_review_per_person_and_product'
  /** unique or primary key constraint on columns "id" */
  | 'product_review_pkey';

/** input type for incrementing numeric columns in table "product_review" */
export type Product_Review_Inc_Input = {
  id?: InputMaybe<Scalars['Int']>;
  product_id?: InputMaybe<Scalars['Int']>;
  rating?: InputMaybe<Scalars['Int']>;
  user_id?: InputMaybe<Scalars['Int']>;
};

/** input type for inserting data into table "product_review" */
export type Product_Review_Insert_Input = {
  comment?: InputMaybe<Scalars['String']>;
  created_at?: InputMaybe<Scalars['timestamptz']>;
  id?: InputMaybe<Scalars['Int']>;
  product?: InputMaybe<Product_Obj_Rel_Insert_Input>;
  product_id?: InputMaybe<Scalars['Int']>;
  rating?: InputMaybe<Scalars['Int']>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
  user?: InputMaybe<User_Obj_Rel_Insert_Input>;
  user_id?: InputMaybe<Scalars['Int']>;
};

/** aggregate max on columns */
export type Product_Review_Max_Fields = {
  comment?: Maybe<Scalars['String']>;
  created_at?: Maybe<Scalars['timestamptz']>;
  id?: Maybe<Scalars['Int']>;
  product_id?: Maybe<Scalars['Int']>;
  rating?: Maybe<Scalars['Int']>;
  updated_at?: Maybe<Scalars['timestamptz']>;
  user_id?: Maybe<Scalars['Int']>;
};

/** order by max() on columns of table "product_review" */
export type Product_Review_Max_Order_By = {
  comment?: InputMaybe<Order_By>;
  created_at?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  rating?: InputMaybe<Order_By>;
  updated_at?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Product_Review_Min_Fields = {
  comment?: Maybe<Scalars['String']>;
  created_at?: Maybe<Scalars['timestamptz']>;
  id?: Maybe<Scalars['Int']>;
  product_id?: Maybe<Scalars['Int']>;
  rating?: Maybe<Scalars['Int']>;
  updated_at?: Maybe<Scalars['timestamptz']>;
  user_id?: Maybe<Scalars['Int']>;
};

/** order by min() on columns of table "product_review" */
export type Product_Review_Min_Order_By = {
  comment?: InputMaybe<Order_By>;
  created_at?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  rating?: InputMaybe<Order_By>;
  updated_at?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "product_review" */
export type Product_Review_Mutation_Response = {
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Product_Review>;
};

/** on_conflict condition type for table "product_review" */
export type Product_Review_On_Conflict = {
  constraint: Product_Review_Constraint;
  update_columns?: Array<Product_Review_Update_Column>;
  where?: InputMaybe<Product_Review_Bool_Exp>;
};

/** Ordering options when selecting data from "product_review". */
export type Product_Review_Order_By = {
  comment?: InputMaybe<Order_By>;
  created_at?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  product?: InputMaybe<Product_Order_By>;
  product_id?: InputMaybe<Order_By>;
  rating?: InputMaybe<Order_By>;
  updated_at?: InputMaybe<Order_By>;
  user?: InputMaybe<User_Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** primary key columns input for table: product_review */
export type Product_Review_Pk_Columns_Input = {
  id: Scalars['Int'];
};

/** select columns of table "product_review" */
export type Product_Review_Select_Column =
  /** column name */
  | 'comment'
  /** column name */
  | 'created_at'
  /** column name */
  | 'id'
  /** column name */
  | 'product_id'
  /** column name */
  | 'rating'
  /** column name */
  | 'updated_at'
  /** column name */
  | 'user_id';

/** input type for updating data in table "product_review" */
export type Product_Review_Set_Input = {
  comment?: InputMaybe<Scalars['String']>;
  created_at?: InputMaybe<Scalars['timestamptz']>;
  id?: InputMaybe<Scalars['Int']>;
  product_id?: InputMaybe<Scalars['Int']>;
  rating?: InputMaybe<Scalars['Int']>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
  user_id?: InputMaybe<Scalars['Int']>;
};

/** aggregate stddev on columns */
export type Product_Review_Stddev_Fields = {
  id?: Maybe<Scalars['Float']>;
  product_id?: Maybe<Scalars['Float']>;
  rating?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "product_review" */
export type Product_Review_Stddev_Order_By = {
  id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  rating?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Product_Review_Stddev_Pop_Fields = {
  id?: Maybe<Scalars['Float']>;
  product_id?: Maybe<Scalars['Float']>;
  rating?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "product_review" */
export type Product_Review_Stddev_Pop_Order_By = {
  id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  rating?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Product_Review_Stddev_Samp_Fields = {
  id?: Maybe<Scalars['Float']>;
  product_id?: Maybe<Scalars['Float']>;
  rating?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "product_review" */
export type Product_Review_Stddev_Samp_Order_By = {
  id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  rating?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** Streaming cursor of the table "product_review" */
export type Product_Review_Stream_Cursor_Input = {
  /** Stream column input with initial value */
  initial_value: Product_Review_Stream_Cursor_Value_Input;
  /** cursor ordering */
  ordering?: InputMaybe<Cursor_Ordering>;
};

/** Initial value of the column from where the streaming should start */
export type Product_Review_Stream_Cursor_Value_Input = {
  comment?: InputMaybe<Scalars['String']>;
  created_at?: InputMaybe<Scalars['timestamptz']>;
  id?: InputMaybe<Scalars['Int']>;
  product_id?: InputMaybe<Scalars['Int']>;
  rating?: InputMaybe<Scalars['Int']>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
  user_id?: InputMaybe<Scalars['Int']>;
};

/** aggregate sum on columns */
export type Product_Review_Sum_Fields = {
  id?: Maybe<Scalars['Int']>;
  product_id?: Maybe<Scalars['Int']>;
  rating?: Maybe<Scalars['Int']>;
  user_id?: Maybe<Scalars['Int']>;
};

/** order by sum() on columns of table "product_review" */
export type Product_Review_Sum_Order_By = {
  id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  rating?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** update columns of table "product_review" */
export type Product_Review_Update_Column =
  /** column name */
  | 'comment'
  /** column name */
  | 'created_at'
  /** column name */
  | 'id'
  /** column name */
  | 'product_id'
  /** column name */
  | 'rating'
  /** column name */
  | 'updated_at'
  /** column name */
  | 'user_id';

export type Product_Review_Updates = {
  /** increments the numeric columns with given value of the filtered values */
  _inc?: InputMaybe<Product_Review_Inc_Input>;
  /** sets the columns of the filtered rows to the given values */
  _set?: InputMaybe<Product_Review_Set_Input>;
  where: Product_Review_Bool_Exp;
};

/** aggregate var_pop on columns */
export type Product_Review_Var_Pop_Fields = {
  id?: Maybe<Scalars['Float']>;
  product_id?: Maybe<Scalars['Float']>;
  rating?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "product_review" */
export type Product_Review_Var_Pop_Order_By = {
  id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  rating?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Product_Review_Var_Samp_Fields = {
  id?: Maybe<Scalars['Float']>;
  product_id?: Maybe<Scalars['Float']>;
  rating?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "product_review" */
export type Product_Review_Var_Samp_Order_By = {
  id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  rating?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Product_Review_Variance_Fields = {
  id?: Maybe<Scalars['Float']>;
  product_id?: Maybe<Scalars['Float']>;
  rating?: Maybe<Scalars['Float']>;
  user_id?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "product_review" */
export type Product_Review_Variance_Order_By = {
  id?: InputMaybe<Order_By>;
  product_id?: InputMaybe<Order_By>;
  rating?: InputMaybe<Order_By>;
  user_id?: InputMaybe<Order_By>;
};

/** select columns of table "product" */
export type Product_Select_Column =
  /** column name */
  | 'brand'
  /** column name */
  | 'category_display_name'
  /** column name */
  | 'created_at'
  /** column name */
  | 'description'
  /** column name */
  | 'id'
  /** column name */
  | 'image_urls'
  /** column name */
  | 'name'
  /** column name */
  | 'price'
  /** column name */
  | 'updated_at';

/** input type for updating data in table "product" */
export type Product_Set_Input = {
  brand?: InputMaybe<Scalars['String']>;
  category_display_name?: InputMaybe<Scalars['String']>;
  created_at?: InputMaybe<Scalars['timestamptz']>;
  description?: InputMaybe<Scalars['String']>;
  id?: InputMaybe<Scalars['Int']>;
  image_urls?: InputMaybe<Scalars['jsonb']>;
  name?: InputMaybe<Scalars['String']>;
  price?: InputMaybe<Scalars['numeric']>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate stddev on columns */
export type Product_Stddev_Fields = {
  id?: Maybe<Scalars['Float']>;
  price?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "product" */
export type Product_Stddev_Order_By = {
  id?: InputMaybe<Order_By>;
  price?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Product_Stddev_Pop_Fields = {
  id?: Maybe<Scalars['Float']>;
  price?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "product" */
export type Product_Stddev_Pop_Order_By = {
  id?: InputMaybe<Order_By>;
  price?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Product_Stddev_Samp_Fields = {
  id?: Maybe<Scalars['Float']>;
  price?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "product" */
export type Product_Stddev_Samp_Order_By = {
  id?: InputMaybe<Order_By>;
  price?: InputMaybe<Order_By>;
};

/** Streaming cursor of the table "product" */
export type Product_Stream_Cursor_Input = {
  /** Stream column input with initial value */
  initial_value: Product_Stream_Cursor_Value_Input;
  /** cursor ordering */
  ordering?: InputMaybe<Cursor_Ordering>;
};

/** Initial value of the column from where the streaming should start */
export type Product_Stream_Cursor_Value_Input = {
  brand?: InputMaybe<Scalars['String']>;
  category_display_name?: InputMaybe<Scalars['String']>;
  created_at?: InputMaybe<Scalars['timestamptz']>;
  description?: InputMaybe<Scalars['String']>;
  id?: InputMaybe<Scalars['Int']>;
  image_urls?: InputMaybe<Scalars['jsonb']>;
  name?: InputMaybe<Scalars['String']>;
  price?: InputMaybe<Scalars['numeric']>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate sum on columns */
export type Product_Sum_Fields = {
  id?: Maybe<Scalars['Int']>;
  price?: Maybe<Scalars['numeric']>;
};

/** order by sum() on columns of table "product" */
export type Product_Sum_Order_By = {
  id?: InputMaybe<Order_By>;
  price?: InputMaybe<Order_By>;
};

/** update columns of table "product" */
export type Product_Update_Column =
  /** column name */
  | 'brand'
  /** column name */
  | 'category_display_name'
  /** column name */
  | 'created_at'
  /** column name */
  | 'description'
  /** column name */
  | 'id'
  /** column name */
  | 'image_urls'
  /** column name */
  | 'name'
  /** column name */
  | 'price'
  /** column name */
  | 'updated_at';

export type Product_Updates = {
  /** append existing jsonb value of filtered columns with new jsonb value */
  _append?: InputMaybe<Product_Append_Input>;
  /** delete the field or element with specified path (for JSON arrays, negative integers count from the end) */
  _delete_at_path?: InputMaybe<Product_Delete_At_Path_Input>;
  /** delete the array element with specified index (negative integers count from the end). throws an error if top level container is not an array */
  _delete_elem?: InputMaybe<Product_Delete_Elem_Input>;
  /** delete key/value pair or string element. key/value pairs are matched based on their key value */
  _delete_key?: InputMaybe<Product_Delete_Key_Input>;
  /** increments the numeric columns with given value of the filtered values */
  _inc?: InputMaybe<Product_Inc_Input>;
  /** prepend existing jsonb value of filtered columns with new jsonb value */
  _prepend?: InputMaybe<Product_Prepend_Input>;
  /** sets the columns of the filtered rows to the given values */
  _set?: InputMaybe<Product_Set_Input>;
  where: Product_Bool_Exp;
};

/** aggregate var_pop on columns */
export type Product_Var_Pop_Fields = {
  id?: Maybe<Scalars['Float']>;
  price?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "product" */
export type Product_Var_Pop_Order_By = {
  id?: InputMaybe<Order_By>;
  price?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Product_Var_Samp_Fields = {
  id?: Maybe<Scalars['Float']>;
  price?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "product" */
export type Product_Var_Samp_Order_By = {
  id?: InputMaybe<Order_By>;
  price?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Product_Variance_Fields = {
  id?: Maybe<Scalars['Float']>;
  price?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "product" */
export type Product_Variance_Order_By = {
  id?: InputMaybe<Order_By>;
  price?: InputMaybe<Order_By>;
};

export type Query_Root = {
  /** fetch data from the table: "address" */
  address: Array<Address>;
  /** fetch aggregated fields from the table: "address" */
  address_aggregate: Address_Aggregate;
  /** fetch data from the table: "address" using primary key columns */
  address_by_pk?: Maybe<Address>;
  adminLogin?: Maybe<Jwt>;
  /** fetch data from the table: "order" */
  order: Array<Order>;
  /** fetch aggregated fields from the table: "order" */
  order_aggregate: Order_Aggregate;
  /** fetch data from the table: "order" using primary key columns */
  order_by_pk?: Maybe<Order>;
  /** fetch data from the table: "order_product" */
  order_product: Array<Order_Product>;
  /** fetch aggregated fields from the table: "order_product" */
  order_product_aggregate: Order_Product_Aggregate;
  /** fetch data from the table: "order_product" using primary key columns */
  order_product_by_pk?: Maybe<Order_Product>;
  /** fetch data from the table: "order_status" */
  order_status: Array<Order_Status>;
  /** fetch aggregated fields from the table: "order_status" */
  order_status_aggregate: Order_Status_Aggregate;
  /** fetch data from the table: "order_status" using primary key columns */
  order_status_by_pk?: Maybe<Order_Status>;
  /** fetch data from the table: "product" */
  product: Array<Product>;
  /** fetch aggregated fields from the table: "product" */
  product_aggregate: Product_Aggregate;
  /** fetch data from the table: "product" using primary key columns */
  product_by_pk?: Maybe<Product>;
  /** fetch data from the table: "product_category_enum" */
  product_category_enum: Array<Product_Category_Enum>;
  /** fetch aggregated fields from the table: "product_category_enum" */
  product_category_enum_aggregate: Product_Category_Enum_Aggregate;
  /** fetch data from the table: "product_category_enum" using primary key columns */
  product_category_enum_by_pk?: Maybe<Product_Category_Enum>;
  /** fetch data from the table: "product_review" */
  product_review: Array<Product_Review>;
  /** fetch aggregated fields from the table: "product_review" */
  product_review_aggregate: Product_Review_Aggregate;
  /** fetch data from the table: "product_review" using primary key columns */
  product_review_by_pk?: Maybe<Product_Review>;
  refreshToken?: Maybe<RefreshTokenJwt>;
  /** fetch data from the table: "site_admin" */
  site_admin: Array<Site_Admin>;
  /** fetch aggregated fields from the table: "site_admin" */
  site_admin_aggregate: Site_Admin_Aggregate;
  /** fetch data from the table: "site_admin" using primary key columns */
  site_admin_by_pk?: Maybe<Site_Admin>;
  /** fetch data from the table: "user" */
  user: Array<User>;
  /** fetch aggregated fields from the table: "user" */
  user_aggregate: User_Aggregate;
  /** fetch data from the table: "user" using primary key columns */
  user_by_pk?: Maybe<User>;
};


export type Query_RootAddressArgs = {
  distinct_on?: InputMaybe<Array<Address_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Address_Order_By>>;
  where?: InputMaybe<Address_Bool_Exp>;
};


export type Query_RootAddress_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Address_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Address_Order_By>>;
  where?: InputMaybe<Address_Bool_Exp>;
};


export type Query_RootAddress_By_PkArgs = {
  id: Scalars['Int'];
};


export type Query_RootAdminLoginArgs = {
  params: AdminLoginInput;
};


export type Query_RootOrderArgs = {
  distinct_on?: InputMaybe<Array<Order_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Order_By>>;
  where?: InputMaybe<Order_Bool_Exp>;
};


export type Query_RootOrder_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Order_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Order_By>>;
  where?: InputMaybe<Order_Bool_Exp>;
};


export type Query_RootOrder_By_PkArgs = {
  id: Scalars['Int'];
};


export type Query_RootOrder_ProductArgs = {
  distinct_on?: InputMaybe<Array<Order_Product_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Product_Order_By>>;
  where?: InputMaybe<Order_Product_Bool_Exp>;
};


export type Query_RootOrder_Product_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Order_Product_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Product_Order_By>>;
  where?: InputMaybe<Order_Product_Bool_Exp>;
};


export type Query_RootOrder_Product_By_PkArgs = {
  id: Scalars['Int'];
};


export type Query_RootOrder_StatusArgs = {
  distinct_on?: InputMaybe<Array<Order_Status_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Status_Order_By>>;
  where?: InputMaybe<Order_Status_Bool_Exp>;
};


export type Query_RootOrder_Status_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Order_Status_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Status_Order_By>>;
  where?: InputMaybe<Order_Status_Bool_Exp>;
};


export type Query_RootOrder_Status_By_PkArgs = {
  status: Scalars['String'];
};


export type Query_RootProductArgs = {
  distinct_on?: InputMaybe<Array<Product_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Product_Order_By>>;
  where?: InputMaybe<Product_Bool_Exp>;
};


export type Query_RootProduct_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Product_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Product_Order_By>>;
  where?: InputMaybe<Product_Bool_Exp>;
};


export type Query_RootProduct_By_PkArgs = {
  id: Scalars['Int'];
};


export type Query_RootProduct_Category_EnumArgs = {
  distinct_on?: InputMaybe<Array<Product_Category_Enum_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Product_Category_Enum_Order_By>>;
  where?: InputMaybe<Product_Category_Enum_Bool_Exp>;
};


export type Query_RootProduct_Category_Enum_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Product_Category_Enum_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Product_Category_Enum_Order_By>>;
  where?: InputMaybe<Product_Category_Enum_Bool_Exp>;
};


export type Query_RootProduct_Category_Enum_By_PkArgs = {
  name: Scalars['String'];
};


export type Query_RootProduct_ReviewArgs = {
  distinct_on?: InputMaybe<Array<Product_Review_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Product_Review_Order_By>>;
  where?: InputMaybe<Product_Review_Bool_Exp>;
};


export type Query_RootProduct_Review_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Product_Review_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Product_Review_Order_By>>;
  where?: InputMaybe<Product_Review_Bool_Exp>;
};


export type Query_RootProduct_Review_By_PkArgs = {
  id: Scalars['Int'];
};


export type Query_RootRefreshTokenArgs = {
  params: RefreshTokenInput;
};


export type Query_RootSite_AdminArgs = {
  distinct_on?: InputMaybe<Array<Site_Admin_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Site_Admin_Order_By>>;
  where?: InputMaybe<Site_Admin_Bool_Exp>;
};


export type Query_RootSite_Admin_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Site_Admin_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Site_Admin_Order_By>>;
  where?: InputMaybe<Site_Admin_Bool_Exp>;
};


export type Query_RootSite_Admin_By_PkArgs = {
  id: Scalars['Int'];
};


export type Query_RootUserArgs = {
  distinct_on?: InputMaybe<Array<User_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<User_Order_By>>;
  where?: InputMaybe<User_Bool_Exp>;
};


export type Query_RootUser_AggregateArgs = {
  distinct_on?: InputMaybe<Array<User_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<User_Order_By>>;
  where?: InputMaybe<User_Bool_Exp>;
};


export type Query_RootUser_By_PkArgs = {
  id: Scalars['Int'];
};

/** Someone administrative capabilities on the site */
export type Site_Admin = {
  created_at: Scalars['timestamptz'];
  email: Scalars['String'];
  id: Scalars['Int'];
  name: Scalars['String'];
  /** A bcrypt-hashed version of the admin password, compared against securely in the JWT Auth API handler for sign-in */
  password: Scalars['String'];
  refresh_token?: Maybe<Scalars['String']>;
  updated_at: Scalars['timestamptz'];
};

/** aggregated selection of "site_admin" */
export type Site_Admin_Aggregate = {
  aggregate?: Maybe<Site_Admin_Aggregate_Fields>;
  nodes: Array<Site_Admin>;
};

/** aggregate fields of "site_admin" */
export type Site_Admin_Aggregate_Fields = {
  avg?: Maybe<Site_Admin_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Site_Admin_Max_Fields>;
  min?: Maybe<Site_Admin_Min_Fields>;
  stddev?: Maybe<Site_Admin_Stddev_Fields>;
  stddev_pop?: Maybe<Site_Admin_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Site_Admin_Stddev_Samp_Fields>;
  sum?: Maybe<Site_Admin_Sum_Fields>;
  var_pop?: Maybe<Site_Admin_Var_Pop_Fields>;
  var_samp?: Maybe<Site_Admin_Var_Samp_Fields>;
  variance?: Maybe<Site_Admin_Variance_Fields>;
};


/** aggregate fields of "site_admin" */
export type Site_Admin_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Site_Admin_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate avg on columns */
export type Site_Admin_Avg_Fields = {
  id?: Maybe<Scalars['Float']>;
};

/** Boolean expression to filter rows from the table "site_admin". All fields are combined with a logical 'AND'. */
export type Site_Admin_Bool_Exp = {
  _and?: InputMaybe<Array<Site_Admin_Bool_Exp>>;
  _not?: InputMaybe<Site_Admin_Bool_Exp>;
  _or?: InputMaybe<Array<Site_Admin_Bool_Exp>>;
  created_at?: InputMaybe<Timestamptz_Comparison_Exp>;
  email?: InputMaybe<String_Comparison_Exp>;
  id?: InputMaybe<Int_Comparison_Exp>;
  name?: InputMaybe<String_Comparison_Exp>;
  password?: InputMaybe<String_Comparison_Exp>;
  refresh_token?: InputMaybe<String_Comparison_Exp>;
  updated_at?: InputMaybe<Timestamptz_Comparison_Exp>;
};

/** unique or primary key constraints on table "site_admin" */
export type Site_Admin_Constraint =
  /** unique or primary key constraint on columns "email" */
  | 'site_admin_email_key'
  /** unique or primary key constraint on columns "id" */
  | 'site_admin_pkey'
  /** unique or primary key constraint on columns "refresh_token" */
  | 'site_admin_refresh_token_key';

/** input type for incrementing numeric columns in table "site_admin" */
export type Site_Admin_Inc_Input = {
  id?: InputMaybe<Scalars['Int']>;
};

/** input type for inserting data into table "site_admin" */
export type Site_Admin_Insert_Input = {
  created_at?: InputMaybe<Scalars['timestamptz']>;
  email?: InputMaybe<Scalars['String']>;
  id?: InputMaybe<Scalars['Int']>;
  name?: InputMaybe<Scalars['String']>;
  /** A bcrypt-hashed version of the admin password, compared against securely in the JWT Auth API handler for sign-in */
  password?: InputMaybe<Scalars['String']>;
  refresh_token?: InputMaybe<Scalars['String']>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate max on columns */
export type Site_Admin_Max_Fields = {
  created_at?: Maybe<Scalars['timestamptz']>;
  email?: Maybe<Scalars['String']>;
  id?: Maybe<Scalars['Int']>;
  name?: Maybe<Scalars['String']>;
  /** A bcrypt-hashed version of the admin password, compared against securely in the JWT Auth API handler for sign-in */
  password?: Maybe<Scalars['String']>;
  refresh_token?: Maybe<Scalars['String']>;
  updated_at?: Maybe<Scalars['timestamptz']>;
};

/** aggregate min on columns */
export type Site_Admin_Min_Fields = {
  created_at?: Maybe<Scalars['timestamptz']>;
  email?: Maybe<Scalars['String']>;
  id?: Maybe<Scalars['Int']>;
  name?: Maybe<Scalars['String']>;
  /** A bcrypt-hashed version of the admin password, compared against securely in the JWT Auth API handler for sign-in */
  password?: Maybe<Scalars['String']>;
  refresh_token?: Maybe<Scalars['String']>;
  updated_at?: Maybe<Scalars['timestamptz']>;
};

/** response of any mutation on the table "site_admin" */
export type Site_Admin_Mutation_Response = {
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Site_Admin>;
};

/** on_conflict condition type for table "site_admin" */
export type Site_Admin_On_Conflict = {
  constraint: Site_Admin_Constraint;
  update_columns?: Array<Site_Admin_Update_Column>;
  where?: InputMaybe<Site_Admin_Bool_Exp>;
};

/** Ordering options when selecting data from "site_admin". */
export type Site_Admin_Order_By = {
  created_at?: InputMaybe<Order_By>;
  email?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  name?: InputMaybe<Order_By>;
  password?: InputMaybe<Order_By>;
  refresh_token?: InputMaybe<Order_By>;
  updated_at?: InputMaybe<Order_By>;
};

/** primary key columns input for table: site_admin */
export type Site_Admin_Pk_Columns_Input = {
  id: Scalars['Int'];
};

/** select columns of table "site_admin" */
export type Site_Admin_Select_Column =
  /** column name */
  | 'created_at'
  /** column name */
  | 'email'
  /** column name */
  | 'id'
  /** column name */
  | 'name'
  /** column name */
  | 'password'
  /** column name */
  | 'refresh_token'
  /** column name */
  | 'updated_at';

/** input type for updating data in table "site_admin" */
export type Site_Admin_Set_Input = {
  created_at?: InputMaybe<Scalars['timestamptz']>;
  email?: InputMaybe<Scalars['String']>;
  id?: InputMaybe<Scalars['Int']>;
  name?: InputMaybe<Scalars['String']>;
  /** A bcrypt-hashed version of the admin password, compared against securely in the JWT Auth API handler for sign-in */
  password?: InputMaybe<Scalars['String']>;
  refresh_token?: InputMaybe<Scalars['String']>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate stddev on columns */
export type Site_Admin_Stddev_Fields = {
  id?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_pop on columns */
export type Site_Admin_Stddev_Pop_Fields = {
  id?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_samp on columns */
export type Site_Admin_Stddev_Samp_Fields = {
  id?: Maybe<Scalars['Float']>;
};

/** Streaming cursor of the table "site_admin" */
export type Site_Admin_Stream_Cursor_Input = {
  /** Stream column input with initial value */
  initial_value: Site_Admin_Stream_Cursor_Value_Input;
  /** cursor ordering */
  ordering?: InputMaybe<Cursor_Ordering>;
};

/** Initial value of the column from where the streaming should start */
export type Site_Admin_Stream_Cursor_Value_Input = {
  created_at?: InputMaybe<Scalars['timestamptz']>;
  email?: InputMaybe<Scalars['String']>;
  id?: InputMaybe<Scalars['Int']>;
  name?: InputMaybe<Scalars['String']>;
  /** A bcrypt-hashed version of the admin password, compared against securely in the JWT Auth API handler for sign-in */
  password?: InputMaybe<Scalars['String']>;
  refresh_token?: InputMaybe<Scalars['String']>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate sum on columns */
export type Site_Admin_Sum_Fields = {
  id?: Maybe<Scalars['Int']>;
};

/** update columns of table "site_admin" */
export type Site_Admin_Update_Column =
  /** column name */
  | 'created_at'
  /** column name */
  | 'email'
  /** column name */
  | 'id'
  /** column name */
  | 'name'
  /** column name */
  | 'password'
  /** column name */
  | 'refresh_token'
  /** column name */
  | 'updated_at';

export type Site_Admin_Updates = {
  /** increments the numeric columns with given value of the filtered values */
  _inc?: InputMaybe<Site_Admin_Inc_Input>;
  /** sets the columns of the filtered rows to the given values */
  _set?: InputMaybe<Site_Admin_Set_Input>;
  where: Site_Admin_Bool_Exp;
};

/** aggregate var_pop on columns */
export type Site_Admin_Var_Pop_Fields = {
  id?: Maybe<Scalars['Float']>;
};

/** aggregate var_samp on columns */
export type Site_Admin_Var_Samp_Fields = {
  id?: Maybe<Scalars['Float']>;
};

/** aggregate variance on columns */
export type Site_Admin_Variance_Fields = {
  id?: Maybe<Scalars['Float']>;
};

export type Subscription_Root = {
  /** fetch data from the table: "address" */
  address: Array<Address>;
  /** fetch aggregated fields from the table: "address" */
  address_aggregate: Address_Aggregate;
  /** fetch data from the table: "address" using primary key columns */
  address_by_pk?: Maybe<Address>;
  /** fetch data from the table in a streaming manner: "address" */
  address_stream: Array<Address>;
  /** fetch data from the table: "order" */
  order: Array<Order>;
  /** fetch aggregated fields from the table: "order" */
  order_aggregate: Order_Aggregate;
  /** fetch data from the table: "order" using primary key columns */
  order_by_pk?: Maybe<Order>;
  /** fetch data from the table: "order_product" */
  order_product: Array<Order_Product>;
  /** fetch aggregated fields from the table: "order_product" */
  order_product_aggregate: Order_Product_Aggregate;
  /** fetch data from the table: "order_product" using primary key columns */
  order_product_by_pk?: Maybe<Order_Product>;
  /** fetch data from the table in a streaming manner: "order_product" */
  order_product_stream: Array<Order_Product>;
  /** fetch data from the table: "order_status" */
  order_status: Array<Order_Status>;
  /** fetch aggregated fields from the table: "order_status" */
  order_status_aggregate: Order_Status_Aggregate;
  /** fetch data from the table: "order_status" using primary key columns */
  order_status_by_pk?: Maybe<Order_Status>;
  /** fetch data from the table in a streaming manner: "order_status" */
  order_status_stream: Array<Order_Status>;
  /** fetch data from the table in a streaming manner: "order" */
  order_stream: Array<Order>;
  /** fetch data from the table: "product" */
  product: Array<Product>;
  /** fetch aggregated fields from the table: "product" */
  product_aggregate: Product_Aggregate;
  /** fetch data from the table: "product" using primary key columns */
  product_by_pk?: Maybe<Product>;
  /** fetch data from the table: "product_category_enum" */
  product_category_enum: Array<Product_Category_Enum>;
  /** fetch aggregated fields from the table: "product_category_enum" */
  product_category_enum_aggregate: Product_Category_Enum_Aggregate;
  /** fetch data from the table: "product_category_enum" using primary key columns */
  product_category_enum_by_pk?: Maybe<Product_Category_Enum>;
  /** fetch data from the table in a streaming manner: "product_category_enum" */
  product_category_enum_stream: Array<Product_Category_Enum>;
  /** fetch data from the table: "product_review" */
  product_review: Array<Product_Review>;
  /** fetch aggregated fields from the table: "product_review" */
  product_review_aggregate: Product_Review_Aggregate;
  /** fetch data from the table: "product_review" using primary key columns */
  product_review_by_pk?: Maybe<Product_Review>;
  /** fetch data from the table in a streaming manner: "product_review" */
  product_review_stream: Array<Product_Review>;
  /** fetch data from the table in a streaming manner: "product" */
  product_stream: Array<Product>;
  /** fetch data from the table: "site_admin" */
  site_admin: Array<Site_Admin>;
  /** fetch aggregated fields from the table: "site_admin" */
  site_admin_aggregate: Site_Admin_Aggregate;
  /** fetch data from the table: "site_admin" using primary key columns */
  site_admin_by_pk?: Maybe<Site_Admin>;
  /** fetch data from the table in a streaming manner: "site_admin" */
  site_admin_stream: Array<Site_Admin>;
  /** fetch data from the table: "user" */
  user: Array<User>;
  /** fetch aggregated fields from the table: "user" */
  user_aggregate: User_Aggregate;
  /** fetch data from the table: "user" using primary key columns */
  user_by_pk?: Maybe<User>;
  /** fetch data from the table in a streaming manner: "user" */
  user_stream: Array<User>;
};


export type Subscription_RootAddressArgs = {
  distinct_on?: InputMaybe<Array<Address_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Address_Order_By>>;
  where?: InputMaybe<Address_Bool_Exp>;
};


export type Subscription_RootAddress_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Address_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Address_Order_By>>;
  where?: InputMaybe<Address_Bool_Exp>;
};


export type Subscription_RootAddress_By_PkArgs = {
  id: Scalars['Int'];
};


export type Subscription_RootAddress_StreamArgs = {
  batch_size: Scalars['Int'];
  cursor: Array<InputMaybe<Address_Stream_Cursor_Input>>;
  where?: InputMaybe<Address_Bool_Exp>;
};


export type Subscription_RootOrderArgs = {
  distinct_on?: InputMaybe<Array<Order_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Order_By>>;
  where?: InputMaybe<Order_Bool_Exp>;
};


export type Subscription_RootOrder_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Order_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Order_By>>;
  where?: InputMaybe<Order_Bool_Exp>;
};


export type Subscription_RootOrder_By_PkArgs = {
  id: Scalars['Int'];
};


export type Subscription_RootOrder_ProductArgs = {
  distinct_on?: InputMaybe<Array<Order_Product_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Product_Order_By>>;
  where?: InputMaybe<Order_Product_Bool_Exp>;
};


export type Subscription_RootOrder_Product_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Order_Product_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Product_Order_By>>;
  where?: InputMaybe<Order_Product_Bool_Exp>;
};


export type Subscription_RootOrder_Product_By_PkArgs = {
  id: Scalars['Int'];
};


export type Subscription_RootOrder_Product_StreamArgs = {
  batch_size: Scalars['Int'];
  cursor: Array<InputMaybe<Order_Product_Stream_Cursor_Input>>;
  where?: InputMaybe<Order_Product_Bool_Exp>;
};


export type Subscription_RootOrder_StatusArgs = {
  distinct_on?: InputMaybe<Array<Order_Status_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Status_Order_By>>;
  where?: InputMaybe<Order_Status_Bool_Exp>;
};


export type Subscription_RootOrder_Status_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Order_Status_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Status_Order_By>>;
  where?: InputMaybe<Order_Status_Bool_Exp>;
};


export type Subscription_RootOrder_Status_By_PkArgs = {
  status: Scalars['String'];
};


export type Subscription_RootOrder_Status_StreamArgs = {
  batch_size: Scalars['Int'];
  cursor: Array<InputMaybe<Order_Status_Stream_Cursor_Input>>;
  where?: InputMaybe<Order_Status_Bool_Exp>;
};


export type Subscription_RootOrder_StreamArgs = {
  batch_size: Scalars['Int'];
  cursor: Array<InputMaybe<Order_Stream_Cursor_Input>>;
  where?: InputMaybe<Order_Bool_Exp>;
};


export type Subscription_RootProductArgs = {
  distinct_on?: InputMaybe<Array<Product_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Product_Order_By>>;
  where?: InputMaybe<Product_Bool_Exp>;
};


export type Subscription_RootProduct_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Product_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Product_Order_By>>;
  where?: InputMaybe<Product_Bool_Exp>;
};


export type Subscription_RootProduct_By_PkArgs = {
  id: Scalars['Int'];
};


export type Subscription_RootProduct_Category_EnumArgs = {
  distinct_on?: InputMaybe<Array<Product_Category_Enum_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Product_Category_Enum_Order_By>>;
  where?: InputMaybe<Product_Category_Enum_Bool_Exp>;
};


export type Subscription_RootProduct_Category_Enum_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Product_Category_Enum_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Product_Category_Enum_Order_By>>;
  where?: InputMaybe<Product_Category_Enum_Bool_Exp>;
};


export type Subscription_RootProduct_Category_Enum_By_PkArgs = {
  name: Scalars['String'];
};


export type Subscription_RootProduct_Category_Enum_StreamArgs = {
  batch_size: Scalars['Int'];
  cursor: Array<InputMaybe<Product_Category_Enum_Stream_Cursor_Input>>;
  where?: InputMaybe<Product_Category_Enum_Bool_Exp>;
};


export type Subscription_RootProduct_ReviewArgs = {
  distinct_on?: InputMaybe<Array<Product_Review_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Product_Review_Order_By>>;
  where?: InputMaybe<Product_Review_Bool_Exp>;
};


export type Subscription_RootProduct_Review_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Product_Review_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Product_Review_Order_By>>;
  where?: InputMaybe<Product_Review_Bool_Exp>;
};


export type Subscription_RootProduct_Review_By_PkArgs = {
  id: Scalars['Int'];
};


export type Subscription_RootProduct_Review_StreamArgs = {
  batch_size: Scalars['Int'];
  cursor: Array<InputMaybe<Product_Review_Stream_Cursor_Input>>;
  where?: InputMaybe<Product_Review_Bool_Exp>;
};


export type Subscription_RootProduct_StreamArgs = {
  batch_size: Scalars['Int'];
  cursor: Array<InputMaybe<Product_Stream_Cursor_Input>>;
  where?: InputMaybe<Product_Bool_Exp>;
};


export type Subscription_RootSite_AdminArgs = {
  distinct_on?: InputMaybe<Array<Site_Admin_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Site_Admin_Order_By>>;
  where?: InputMaybe<Site_Admin_Bool_Exp>;
};


export type Subscription_RootSite_Admin_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Site_Admin_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Site_Admin_Order_By>>;
  where?: InputMaybe<Site_Admin_Bool_Exp>;
};


export type Subscription_RootSite_Admin_By_PkArgs = {
  id: Scalars['Int'];
};


export type Subscription_RootSite_Admin_StreamArgs = {
  batch_size: Scalars['Int'];
  cursor: Array<InputMaybe<Site_Admin_Stream_Cursor_Input>>;
  where?: InputMaybe<Site_Admin_Bool_Exp>;
};


export type Subscription_RootUserArgs = {
  distinct_on?: InputMaybe<Array<User_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<User_Order_By>>;
  where?: InputMaybe<User_Bool_Exp>;
};


export type Subscription_RootUser_AggregateArgs = {
  distinct_on?: InputMaybe<Array<User_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<User_Order_By>>;
  where?: InputMaybe<User_Bool_Exp>;
};


export type Subscription_RootUser_By_PkArgs = {
  id: Scalars['Int'];
};


export type Subscription_RootUser_StreamArgs = {
  batch_size: Scalars['Int'];
  cursor: Array<InputMaybe<User_Stream_Cursor_Input>>;
  where?: InputMaybe<User_Bool_Exp>;
};

/** Boolean expression to compare columns of type "timestamptz". All fields are combined with logical 'AND'. */
export type Timestamptz_Comparison_Exp = {
  _eq?: InputMaybe<Scalars['timestamptz']>;
  _gt?: InputMaybe<Scalars['timestamptz']>;
  _gte?: InputMaybe<Scalars['timestamptz']>;
  _in?: InputMaybe<Array<Scalars['timestamptz']>>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  _lt?: InputMaybe<Scalars['timestamptz']>;
  _lte?: InputMaybe<Scalars['timestamptz']>;
  _neq?: InputMaybe<Scalars['timestamptz']>;
  _nin?: InputMaybe<Array<Scalars['timestamptz']>>;
};

/** Someone with an account on the site, who uses it to make purchases */
export type User = {
  /** An array relationship */
  addresses: Array<Address>;
  /** An aggregate relationship */
  addresses_aggregate: Address_Aggregate;
  created_at: Scalars['timestamptz'];
  email: Scalars['String'];
  id: Scalars['Int'];
  name: Scalars['String'];
  /** An array relationship */
  orders: Array<Order>;
  /** An aggregate relationship */
  orders_aggregate: Order_Aggregate;
  /** A bcrypt-hashed version of the user password, compared against securely in the JWT Auth API handler for sign-in */
  password: Scalars['String'];
  /** An array relationship */
  product_reviews: Array<Product_Review>;
  /** An aggregate relationship */
  product_reviews_aggregate: Product_Review_Aggregate;
  refresh_token?: Maybe<Scalars['String']>;
  updated_at: Scalars['timestamptz'];
};


/** Someone with an account on the site, who uses it to make purchases */
export type UserAddressesArgs = {
  distinct_on?: InputMaybe<Array<Address_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Address_Order_By>>;
  where?: InputMaybe<Address_Bool_Exp>;
};


/** Someone with an account on the site, who uses it to make purchases */
export type UserAddresses_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Address_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Address_Order_By>>;
  where?: InputMaybe<Address_Bool_Exp>;
};


/** Someone with an account on the site, who uses it to make purchases */
export type UserOrdersArgs = {
  distinct_on?: InputMaybe<Array<Order_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Order_By>>;
  where?: InputMaybe<Order_Bool_Exp>;
};


/** Someone with an account on the site, who uses it to make purchases */
export type UserOrders_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Order_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Order_Order_By>>;
  where?: InputMaybe<Order_Bool_Exp>;
};


/** Someone with an account on the site, who uses it to make purchases */
export type UserProduct_ReviewsArgs = {
  distinct_on?: InputMaybe<Array<Product_Review_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Product_Review_Order_By>>;
  where?: InputMaybe<Product_Review_Bool_Exp>;
};


/** Someone with an account on the site, who uses it to make purchases */
export type UserProduct_Reviews_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Product_Review_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Product_Review_Order_By>>;
  where?: InputMaybe<Product_Review_Bool_Exp>;
};

/** aggregated selection of "user" */
export type User_Aggregate = {
  aggregate?: Maybe<User_Aggregate_Fields>;
  nodes: Array<User>;
};

/** aggregate fields of "user" */
export type User_Aggregate_Fields = {
  avg?: Maybe<User_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<User_Max_Fields>;
  min?: Maybe<User_Min_Fields>;
  stddev?: Maybe<User_Stddev_Fields>;
  stddev_pop?: Maybe<User_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<User_Stddev_Samp_Fields>;
  sum?: Maybe<User_Sum_Fields>;
  var_pop?: Maybe<User_Var_Pop_Fields>;
  var_samp?: Maybe<User_Var_Samp_Fields>;
  variance?: Maybe<User_Variance_Fields>;
};


/** aggregate fields of "user" */
export type User_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<User_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate avg on columns */
export type User_Avg_Fields = {
  id?: Maybe<Scalars['Float']>;
};

/** Boolean expression to filter rows from the table "user". All fields are combined with a logical 'AND'. */
export type User_Bool_Exp = {
  _and?: InputMaybe<Array<User_Bool_Exp>>;
  _not?: InputMaybe<User_Bool_Exp>;
  _or?: InputMaybe<Array<User_Bool_Exp>>;
  addresses?: InputMaybe<Address_Bool_Exp>;
  addresses_aggregate?: InputMaybe<Address_Aggregate_Bool_Exp>;
  created_at?: InputMaybe<Timestamptz_Comparison_Exp>;
  email?: InputMaybe<String_Comparison_Exp>;
  id?: InputMaybe<Int_Comparison_Exp>;
  name?: InputMaybe<String_Comparison_Exp>;
  orders?: InputMaybe<Order_Bool_Exp>;
  orders_aggregate?: InputMaybe<Order_Aggregate_Bool_Exp>;
  password?: InputMaybe<String_Comparison_Exp>;
  product_reviews?: InputMaybe<Product_Review_Bool_Exp>;
  product_reviews_aggregate?: InputMaybe<Product_Review_Aggregate_Bool_Exp>;
  refresh_token?: InputMaybe<String_Comparison_Exp>;
  updated_at?: InputMaybe<Timestamptz_Comparison_Exp>;
};

/** unique or primary key constraints on table "user" */
export type User_Constraint =
  /** unique or primary key constraint on columns "email" */
  | 'user_email_key'
  /** unique or primary key constraint on columns "id" */
  | 'user_pkey'
  /** unique or primary key constraint on columns "refresh_token" */
  | 'user_refresh_token_key';

/** input type for incrementing numeric columns in table "user" */
export type User_Inc_Input = {
  id?: InputMaybe<Scalars['Int']>;
};

/** input type for inserting data into table "user" */
export type User_Insert_Input = {
  addresses?: InputMaybe<Address_Arr_Rel_Insert_Input>;
  created_at?: InputMaybe<Scalars['timestamptz']>;
  email?: InputMaybe<Scalars['String']>;
  id?: InputMaybe<Scalars['Int']>;
  name?: InputMaybe<Scalars['String']>;
  orders?: InputMaybe<Order_Arr_Rel_Insert_Input>;
  /** A bcrypt-hashed version of the user password, compared against securely in the JWT Auth API handler for sign-in */
  password?: InputMaybe<Scalars['String']>;
  product_reviews?: InputMaybe<Product_Review_Arr_Rel_Insert_Input>;
  refresh_token?: InputMaybe<Scalars['String']>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate max on columns */
export type User_Max_Fields = {
  created_at?: Maybe<Scalars['timestamptz']>;
  email?: Maybe<Scalars['String']>;
  id?: Maybe<Scalars['Int']>;
  name?: Maybe<Scalars['String']>;
  /** A bcrypt-hashed version of the user password, compared against securely in the JWT Auth API handler for sign-in */
  password?: Maybe<Scalars['String']>;
  refresh_token?: Maybe<Scalars['String']>;
  updated_at?: Maybe<Scalars['timestamptz']>;
};

/** aggregate min on columns */
export type User_Min_Fields = {
  created_at?: Maybe<Scalars['timestamptz']>;
  email?: Maybe<Scalars['String']>;
  id?: Maybe<Scalars['Int']>;
  name?: Maybe<Scalars['String']>;
  /** A bcrypt-hashed version of the user password, compared against securely in the JWT Auth API handler for sign-in */
  password?: Maybe<Scalars['String']>;
  refresh_token?: Maybe<Scalars['String']>;
  updated_at?: Maybe<Scalars['timestamptz']>;
};

/** response of any mutation on the table "user" */
export type User_Mutation_Response = {
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<User>;
};

/** input type for inserting object relation for remote table "user" */
export type User_Obj_Rel_Insert_Input = {
  data: User_Insert_Input;
  /** upsert condition */
  on_conflict?: InputMaybe<User_On_Conflict>;
};

/** on_conflict condition type for table "user" */
export type User_On_Conflict = {
  constraint: User_Constraint;
  update_columns?: Array<User_Update_Column>;
  where?: InputMaybe<User_Bool_Exp>;
};

/** Ordering options when selecting data from "user". */
export type User_Order_By = {
  addresses_aggregate?: InputMaybe<Address_Aggregate_Order_By>;
  created_at?: InputMaybe<Order_By>;
  email?: InputMaybe<Order_By>;
  id?: InputMaybe<Order_By>;
  name?: InputMaybe<Order_By>;
  orders_aggregate?: InputMaybe<Order_Aggregate_Order_By>;
  password?: InputMaybe<Order_By>;
  product_reviews_aggregate?: InputMaybe<Product_Review_Aggregate_Order_By>;
  refresh_token?: InputMaybe<Order_By>;
  updated_at?: InputMaybe<Order_By>;
};

/** primary key columns input for table: user */
export type User_Pk_Columns_Input = {
  id: Scalars['Int'];
};

/** select columns of table "user" */
export type User_Select_Column =
  /** column name */
  | 'created_at'
  /** column name */
  | 'email'
  /** column name */
  | 'id'
  /** column name */
  | 'name'
  /** column name */
  | 'password'
  /** column name */
  | 'refresh_token'
  /** column name */
  | 'updated_at';

/** input type for updating data in table "user" */
export type User_Set_Input = {
  created_at?: InputMaybe<Scalars['timestamptz']>;
  email?: InputMaybe<Scalars['String']>;
  id?: InputMaybe<Scalars['Int']>;
  name?: InputMaybe<Scalars['String']>;
  /** A bcrypt-hashed version of the user password, compared against securely in the JWT Auth API handler for sign-in */
  password?: InputMaybe<Scalars['String']>;
  refresh_token?: InputMaybe<Scalars['String']>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate stddev on columns */
export type User_Stddev_Fields = {
  id?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_pop on columns */
export type User_Stddev_Pop_Fields = {
  id?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_samp on columns */
export type User_Stddev_Samp_Fields = {
  id?: Maybe<Scalars['Float']>;
};

/** Streaming cursor of the table "user" */
export type User_Stream_Cursor_Input = {
  /** Stream column input with initial value */
  initial_value: User_Stream_Cursor_Value_Input;
  /** cursor ordering */
  ordering?: InputMaybe<Cursor_Ordering>;
};

/** Initial value of the column from where the streaming should start */
export type User_Stream_Cursor_Value_Input = {
  created_at?: InputMaybe<Scalars['timestamptz']>;
  email?: InputMaybe<Scalars['String']>;
  id?: InputMaybe<Scalars['Int']>;
  name?: InputMaybe<Scalars['String']>;
  /** A bcrypt-hashed version of the user password, compared against securely in the JWT Auth API handler for sign-in */
  password?: InputMaybe<Scalars['String']>;
  refresh_token?: InputMaybe<Scalars['String']>;
  updated_at?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate sum on columns */
export type User_Sum_Fields = {
  id?: Maybe<Scalars['Int']>;
};

/** update columns of table "user" */
export type User_Update_Column =
  /** column name */
  | 'created_at'
  /** column name */
  | 'email'
  /** column name */
  | 'id'
  /** column name */
  | 'name'
  /** column name */
  | 'password'
  /** column name */
  | 'refresh_token'
  /** column name */
  | 'updated_at';

export type User_Updates = {
  /** increments the numeric columns with given value of the filtered values */
  _inc?: InputMaybe<User_Inc_Input>;
  /** sets the columns of the filtered rows to the given values */
  _set?: InputMaybe<User_Set_Input>;
  where: User_Bool_Exp;
};

/** aggregate var_pop on columns */
export type User_Var_Pop_Fields = {
  id?: Maybe<Scalars['Float']>;
};

/** aggregate var_samp on columns */
export type User_Var_Samp_Fields = {
  id?: Maybe<Scalars['Float']>;
};

/** aggregate variance on columns */
export type User_Variance_Fields = {
  id?: Maybe<Scalars['Float']>;
};

export type GetProductsQueryVariables = Exact<{ [key: string]: never; }>;


export type GetProductsQuery = { product: Array<{ id: number, name: string }> };

export type GetProductQueryVariables = Exact<{
  id: Scalars['Int'];
}>;


export type GetProductQuery = { product_by_pk?: { id: number, description?: string | null, name: string, price: number, image_urls?: any | null } | null };

export type PlaceOrderMutationVariables = Exact<{
  products: Order_Product_Arr_Rel_Insert_Input;
}>;


export type PlaceOrderMutation = { insert_order_one?: { id: number } | null };


export const GetProductsDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"GetProducts"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"product"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"limit"},"value":{"kind":"IntValue","value":"10"}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"id"}},{"kind":"Field","name":{"kind":"Name","value":"name"}}]}}]}}]} as unknown as DocumentNode<GetProductsQuery, GetProductsQueryVariables>;
export const GetProductDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"GetProduct"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"product_by_pk"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"id"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"id"}},{"kind":"Field","name":{"kind":"Name","value":"description"}},{"kind":"Field","name":{"kind":"Name","value":"name"}},{"kind":"Field","name":{"kind":"Name","value":"price"}},{"kind":"Field","name":{"kind":"Name","value":"image_urls"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"path"},"value":{"kind":"StringValue","value":"$[0]","block":false}}]}]}}]}}]} as unknown as DocumentNode<GetProductQuery, GetProductQueryVariables>;
export const PlaceOrderDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"PlaceOrder"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"products"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"order_product_arr_rel_insert_input"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"insert_order_one"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"object"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"products"},"value":{"kind":"Variable","name":{"kind":"Name","value":"products"}}},{"kind":"ObjectField","name":{"kind":"Name","value":"billing_address_id"},"value":{"kind":"IntValue","value":"222"}},{"kind":"ObjectField","name":{"kind":"Name","value":"user_id"},"value":{"kind":"IntValue","value":"225"}},{"kind":"ObjectField","name":{"kind":"Name","value":"shipping_address_id"},"value":{"kind":"IntValue","value":"222"}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"id"}}]}}]}}]} as unknown as DocumentNode<PlaceOrderMutation, PlaceOrderMutationVariables>;