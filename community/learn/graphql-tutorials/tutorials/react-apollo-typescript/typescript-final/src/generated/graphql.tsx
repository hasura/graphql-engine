import gql from 'graphql-tag';
import * as ApolloReactCommon from '@apollo/react-common';
import * as React from 'react';
import * as ApolloReactComponents from '@apollo/react-components';
import * as ApolloReactHoc from '@apollo/react-hoc';
export type Maybe<T> = T | null;
export type Omit<T, K extends keyof T> = Pick<T, Exclude<keyof T, K>>;
/** All built-in and custom scalars, mapped to their actual values */
export type Scalars = {
  ID: string,
  String: string,
  Boolean: boolean,
  Int: number,
  Float: number,
  timestamptz: any,
};

/** expression to compare columns of type boolean. All fields are combined with logical 'AND'. */
export type Boolean_Comparison_Exp = {
  _eq?: Maybe<Scalars['Boolean']>,
  _gt?: Maybe<Scalars['Boolean']>,
  _gte?: Maybe<Scalars['Boolean']>,
  _in?: Maybe<Array<Scalars['Boolean']>>,
  _is_null?: Maybe<Scalars['Boolean']>,
  _lt?: Maybe<Scalars['Boolean']>,
  _lte?: Maybe<Scalars['Boolean']>,
  _neq?: Maybe<Scalars['Boolean']>,
  _nin?: Maybe<Array<Scalars['Boolean']>>,
};

/** conflict action */
export enum Conflict_Action {
  /** ignore the insert on this row */
  Ignore = 'ignore',
  /** update the row with the given values */
  Update = 'update'
}

/** expression to compare columns of type integer. All fields are combined with logical 'AND'. */
export type Integer_Comparison_Exp = {
  _eq?: Maybe<Scalars['Int']>,
  _gt?: Maybe<Scalars['Int']>,
  _gte?: Maybe<Scalars['Int']>,
  _in?: Maybe<Array<Scalars['Int']>>,
  _is_null?: Maybe<Scalars['Boolean']>,
  _lt?: Maybe<Scalars['Int']>,
  _lte?: Maybe<Scalars['Int']>,
  _neq?: Maybe<Scalars['Int']>,
  _nin?: Maybe<Array<Scalars['Int']>>,
};

/** mutation root */
export type Mutation_Root = {
   __typename?: 'mutation_root',
  /** delete data from the table: "online_users" */
  delete_online_users?: Maybe<Online_Users_Mutation_Response>,
  /** delete data from the table: "todos" */
  delete_todos?: Maybe<Todos_Mutation_Response>,
  /** delete data from the table: "users" */
  delete_users?: Maybe<Users_Mutation_Response>,
  /** insert data into the table: "online_users" */
  insert_online_users?: Maybe<Online_Users_Mutation_Response>,
  /** insert data into the table: "todos" */
  insert_todos?: Maybe<Todos_Mutation_Response>,
  /** insert data into the table: "users" */
  insert_users?: Maybe<Users_Mutation_Response>,
  /** update data of the table: "online_users" */
  update_online_users?: Maybe<Online_Users_Mutation_Response>,
  /** update data of the table: "todos" */
  update_todos?: Maybe<Todos_Mutation_Response>,
  /** update data of the table: "users" */
  update_users?: Maybe<Users_Mutation_Response>,
};


/** mutation root */
export type Mutation_RootDelete_Online_UsersArgs = {
  where: Online_Users_Bool_Exp
};


/** mutation root */
export type Mutation_RootDelete_TodosArgs = {
  where: Todos_Bool_Exp
};


/** mutation root */
export type Mutation_RootDelete_UsersArgs = {
  where: Users_Bool_Exp
};


/** mutation root */
export type Mutation_RootInsert_Online_UsersArgs = {
  objects: Array<Online_Users_Insert_Input>
};


/** mutation root */
export type Mutation_RootInsert_TodosArgs = {
  objects: Array<Todos_Insert_Input>,
  on_conflict?: Maybe<Todos_On_Conflict>
};


/** mutation root */
export type Mutation_RootInsert_UsersArgs = {
  objects: Array<Users_Insert_Input>,
  on_conflict?: Maybe<Users_On_Conflict>
};


/** mutation root */
export type Mutation_RootUpdate_Online_UsersArgs = {
  _set?: Maybe<Online_Users_Set_Input>,
  where: Online_Users_Bool_Exp
};


/** mutation root */
export type Mutation_RootUpdate_TodosArgs = {
  _inc?: Maybe<Todos_Inc_Input>,
  _set?: Maybe<Todos_Set_Input>,
  where: Todos_Bool_Exp
};


/** mutation root */
export type Mutation_RootUpdate_UsersArgs = {
  _set?: Maybe<Users_Set_Input>,
  where: Users_Bool_Exp
};

/** columns and relationships of "online_users" */
export type Online_Users = {
   __typename?: 'online_users',
  id?: Maybe<Scalars['String']>,
  last_seen?: Maybe<Scalars['timestamptz']>,
  /** An object relationship */
  user?: Maybe<Users>,
};

/** aggregated selection of "online_users" */
export type Online_Users_Aggregate = {
   __typename?: 'online_users_aggregate',
  aggregate?: Maybe<Online_Users_Aggregate_Fields>,
  nodes: Array<Online_Users>,
};

/** aggregate fields of "online_users" */
export type Online_Users_Aggregate_Fields = {
   __typename?: 'online_users_aggregate_fields',
  count?: Maybe<Scalars['Int']>,
  max?: Maybe<Online_Users_Max_Fields>,
  min?: Maybe<Online_Users_Min_Fields>,
};


/** aggregate fields of "online_users" */
export type Online_Users_Aggregate_FieldsCountArgs = {
  columns?: Maybe<Array<Online_Users_Select_Column>>,
  distinct?: Maybe<Scalars['Boolean']>
};

/** order by aggregate values of table "online_users" */
export type Online_Users_Aggregate_Order_By = {
  count?: Maybe<Order_By>,
  max?: Maybe<Online_Users_Max_Order_By>,
  min?: Maybe<Online_Users_Min_Order_By>,
};

/** input type for inserting array relation for remote table "online_users" */
export type Online_Users_Arr_Rel_Insert_Input = {
  data: Array<Online_Users_Insert_Input>,
};

/** Boolean expression to filter rows from the table "online_users". All fields are combined with a logical 'AND'. */
export type Online_Users_Bool_Exp = {
  _and?: Maybe<Array<Maybe<Online_Users_Bool_Exp>>>,
  _not?: Maybe<Online_Users_Bool_Exp>,
  _or?: Maybe<Array<Maybe<Online_Users_Bool_Exp>>>,
  id?: Maybe<Text_Comparison_Exp>,
  last_seen?: Maybe<Timestamptz_Comparison_Exp>,
  user?: Maybe<Users_Bool_Exp>,
};

/** input type for inserting data into table "online_users" */
export type Online_Users_Insert_Input = {
  id?: Maybe<Scalars['String']>,
  last_seen?: Maybe<Scalars['timestamptz']>,
  user?: Maybe<Users_Obj_Rel_Insert_Input>,
};

/** aggregate max on columns */
export type Online_Users_Max_Fields = {
   __typename?: 'online_users_max_fields',
  id?: Maybe<Scalars['String']>,
  last_seen?: Maybe<Scalars['timestamptz']>,
};

/** order by max() on columns of table "online_users" */
export type Online_Users_Max_Order_By = {
  id?: Maybe<Order_By>,
  last_seen?: Maybe<Order_By>,
};

/** aggregate min on columns */
export type Online_Users_Min_Fields = {
   __typename?: 'online_users_min_fields',
  id?: Maybe<Scalars['String']>,
  last_seen?: Maybe<Scalars['timestamptz']>,
};

/** order by min() on columns of table "online_users" */
export type Online_Users_Min_Order_By = {
  id?: Maybe<Order_By>,
  last_seen?: Maybe<Order_By>,
};

/** response of any mutation on the table "online_users" */
export type Online_Users_Mutation_Response = {
   __typename?: 'online_users_mutation_response',
  /** number of affected rows by the mutation */
  affected_rows: Scalars['Int'],
  /** data of the affected rows by the mutation */
  returning: Array<Online_Users>,
};

/** input type for inserting object relation for remote table "online_users" */
export type Online_Users_Obj_Rel_Insert_Input = {
  data: Online_Users_Insert_Input,
};

/** ordering options when selecting data from "online_users" */
export type Online_Users_Order_By = {
  id?: Maybe<Order_By>,
  last_seen?: Maybe<Order_By>,
  user?: Maybe<Users_Order_By>,
};

/** select columns of table "online_users" */
export enum Online_Users_Select_Column {
  /** column name */
  Id = 'id',
  /** column name */
  LastSeen = 'last_seen'
}

/** input type for updating data in table "online_users" */
export type Online_Users_Set_Input = {
  id?: Maybe<Scalars['String']>,
  last_seen?: Maybe<Scalars['timestamptz']>,
};

/** column ordering options */
export enum Order_By {
  /** in the ascending order, nulls last */
  Asc = 'asc',
  /** in the ascending order, nulls first */
  AscNullsFirst = 'asc_nulls_first',
  /** in the ascending order, nulls last */
  AscNullsLast = 'asc_nulls_last',
  /** in the descending order, nulls first */
  Desc = 'desc',
  /** in the descending order, nulls first */
  DescNullsFirst = 'desc_nulls_first',
  /** in the descending order, nulls last */
  DescNullsLast = 'desc_nulls_last'
}

/** query root */
export type Query_Root = {
   __typename?: 'query_root',
  /** fetch data from the table: "online_users" */
  online_users: Array<Online_Users>,
  /** fetch aggregated fields from the table: "online_users" */
  online_users_aggregate: Online_Users_Aggregate,
  /** fetch data from the table: "todos" */
  todos: Array<Todos>,
  /** fetch aggregated fields from the table: "todos" */
  todos_aggregate: Todos_Aggregate,
  /** fetch data from the table: "todos" using primary key columns */
  todos_by_pk?: Maybe<Todos>,
  /** fetch data from the table: "users" */
  users: Array<Users>,
  /** fetch aggregated fields from the table: "users" */
  users_aggregate: Users_Aggregate,
  /** fetch data from the table: "users" using primary key columns */
  users_by_pk?: Maybe<Users>,
};


/** query root */
export type Query_RootOnline_UsersArgs = {
  distinct_on?: Maybe<Array<Online_Users_Select_Column>>,
  limit?: Maybe<Scalars['Int']>,
  offset?: Maybe<Scalars['Int']>,
  order_by?: Maybe<Array<Online_Users_Order_By>>,
  where?: Maybe<Online_Users_Bool_Exp>
};


/** query root */
export type Query_RootOnline_Users_AggregateArgs = {
  distinct_on?: Maybe<Array<Online_Users_Select_Column>>,
  limit?: Maybe<Scalars['Int']>,
  offset?: Maybe<Scalars['Int']>,
  order_by?: Maybe<Array<Online_Users_Order_By>>,
  where?: Maybe<Online_Users_Bool_Exp>
};


/** query root */
export type Query_RootTodosArgs = {
  distinct_on?: Maybe<Array<Todos_Select_Column>>,
  limit?: Maybe<Scalars['Int']>,
  offset?: Maybe<Scalars['Int']>,
  order_by?: Maybe<Array<Todos_Order_By>>,
  where?: Maybe<Todos_Bool_Exp>
};


/** query root */
export type Query_RootTodos_AggregateArgs = {
  distinct_on?: Maybe<Array<Todos_Select_Column>>,
  limit?: Maybe<Scalars['Int']>,
  offset?: Maybe<Scalars['Int']>,
  order_by?: Maybe<Array<Todos_Order_By>>,
  where?: Maybe<Todos_Bool_Exp>
};


/** query root */
export type Query_RootTodos_By_PkArgs = {
  id: Scalars['Int']
};


/** query root */
export type Query_RootUsersArgs = {
  distinct_on?: Maybe<Array<Users_Select_Column>>,
  limit?: Maybe<Scalars['Int']>,
  offset?: Maybe<Scalars['Int']>,
  order_by?: Maybe<Array<Users_Order_By>>,
  where?: Maybe<Users_Bool_Exp>
};


/** query root */
export type Query_RootUsers_AggregateArgs = {
  distinct_on?: Maybe<Array<Users_Select_Column>>,
  limit?: Maybe<Scalars['Int']>,
  offset?: Maybe<Scalars['Int']>,
  order_by?: Maybe<Array<Users_Order_By>>,
  where?: Maybe<Users_Bool_Exp>
};


/** query root */
export type Query_RootUsers_By_PkArgs = {
  id: Scalars['String']
};

/** subscription root */
export type Subscription_Root = {
   __typename?: 'subscription_root',
  /** fetch data from the table: "online_users" */
  online_users: Array<Online_Users>,
  /** fetch aggregated fields from the table: "online_users" */
  online_users_aggregate: Online_Users_Aggregate,
  /** fetch data from the table: "todos" */
  todos: Array<Todos>,
  /** fetch aggregated fields from the table: "todos" */
  todos_aggregate: Todos_Aggregate,
  /** fetch data from the table: "todos" using primary key columns */
  todos_by_pk?: Maybe<Todos>,
  /** fetch data from the table: "users" */
  users: Array<Users>,
  /** fetch aggregated fields from the table: "users" */
  users_aggregate: Users_Aggregate,
  /** fetch data from the table: "users" using primary key columns */
  users_by_pk?: Maybe<Users>,
};


/** subscription root */
export type Subscription_RootOnline_UsersArgs = {
  distinct_on?: Maybe<Array<Online_Users_Select_Column>>,
  limit?: Maybe<Scalars['Int']>,
  offset?: Maybe<Scalars['Int']>,
  order_by?: Maybe<Array<Online_Users_Order_By>>,
  where?: Maybe<Online_Users_Bool_Exp>
};


/** subscription root */
export type Subscription_RootOnline_Users_AggregateArgs = {
  distinct_on?: Maybe<Array<Online_Users_Select_Column>>,
  limit?: Maybe<Scalars['Int']>,
  offset?: Maybe<Scalars['Int']>,
  order_by?: Maybe<Array<Online_Users_Order_By>>,
  where?: Maybe<Online_Users_Bool_Exp>
};


/** subscription root */
export type Subscription_RootTodosArgs = {
  distinct_on?: Maybe<Array<Todos_Select_Column>>,
  limit?: Maybe<Scalars['Int']>,
  offset?: Maybe<Scalars['Int']>,
  order_by?: Maybe<Array<Todos_Order_By>>,
  where?: Maybe<Todos_Bool_Exp>
};


/** subscription root */
export type Subscription_RootTodos_AggregateArgs = {
  distinct_on?: Maybe<Array<Todos_Select_Column>>,
  limit?: Maybe<Scalars['Int']>,
  offset?: Maybe<Scalars['Int']>,
  order_by?: Maybe<Array<Todos_Order_By>>,
  where?: Maybe<Todos_Bool_Exp>
};


/** subscription root */
export type Subscription_RootTodos_By_PkArgs = {
  id: Scalars['Int']
};


/** subscription root */
export type Subscription_RootUsersArgs = {
  distinct_on?: Maybe<Array<Users_Select_Column>>,
  limit?: Maybe<Scalars['Int']>,
  offset?: Maybe<Scalars['Int']>,
  order_by?: Maybe<Array<Users_Order_By>>,
  where?: Maybe<Users_Bool_Exp>
};


/** subscription root */
export type Subscription_RootUsers_AggregateArgs = {
  distinct_on?: Maybe<Array<Users_Select_Column>>,
  limit?: Maybe<Scalars['Int']>,
  offset?: Maybe<Scalars['Int']>,
  order_by?: Maybe<Array<Users_Order_By>>,
  where?: Maybe<Users_Bool_Exp>
};


/** subscription root */
export type Subscription_RootUsers_By_PkArgs = {
  id: Scalars['String']
};

/** expression to compare columns of type text. All fields are combined with logical 'AND'. */
export type Text_Comparison_Exp = {
  _eq?: Maybe<Scalars['String']>,
  _gt?: Maybe<Scalars['String']>,
  _gte?: Maybe<Scalars['String']>,
  _ilike?: Maybe<Scalars['String']>,
  _in?: Maybe<Array<Scalars['String']>>,
  _is_null?: Maybe<Scalars['Boolean']>,
  _like?: Maybe<Scalars['String']>,
  _lt?: Maybe<Scalars['String']>,
  _lte?: Maybe<Scalars['String']>,
  _neq?: Maybe<Scalars['String']>,
  _nilike?: Maybe<Scalars['String']>,
  _nin?: Maybe<Array<Scalars['String']>>,
  _nlike?: Maybe<Scalars['String']>,
  _nsimilar?: Maybe<Scalars['String']>,
  _similar?: Maybe<Scalars['String']>,
};


/** expression to compare columns of type timestamptz. All fields are combined with logical 'AND'. */
export type Timestamptz_Comparison_Exp = {
  _eq?: Maybe<Scalars['timestamptz']>,
  _gt?: Maybe<Scalars['timestamptz']>,
  _gte?: Maybe<Scalars['timestamptz']>,
  _in?: Maybe<Array<Scalars['timestamptz']>>,
  _is_null?: Maybe<Scalars['Boolean']>,
  _lt?: Maybe<Scalars['timestamptz']>,
  _lte?: Maybe<Scalars['timestamptz']>,
  _neq?: Maybe<Scalars['timestamptz']>,
  _nin?: Maybe<Array<Scalars['timestamptz']>>,
};

/** columns and relationships of "todos" */
export type Todos = {
   __typename?: 'todos',
  created_at: Scalars['timestamptz'],
  id: Scalars['Int'],
  is_completed: Scalars['Boolean'],
  is_public: Scalars['Boolean'],
  title: Scalars['String'],
  /** An object relationship */
  user: Users,
  user_id: Scalars['String'],
};

/** aggregated selection of "todos" */
export type Todos_Aggregate = {
   __typename?: 'todos_aggregate',
  aggregate?: Maybe<Todos_Aggregate_Fields>,
  nodes: Array<Todos>,
};

/** aggregate fields of "todos" */
export type Todos_Aggregate_Fields = {
   __typename?: 'todos_aggregate_fields',
  avg?: Maybe<Todos_Avg_Fields>,
  count?: Maybe<Scalars['Int']>,
  max?: Maybe<Todos_Max_Fields>,
  min?: Maybe<Todos_Min_Fields>,
  stddev?: Maybe<Todos_Stddev_Fields>,
  stddev_pop?: Maybe<Todos_Stddev_Pop_Fields>,
  stddev_samp?: Maybe<Todos_Stddev_Samp_Fields>,
  sum?: Maybe<Todos_Sum_Fields>,
  var_pop?: Maybe<Todos_Var_Pop_Fields>,
  var_samp?: Maybe<Todos_Var_Samp_Fields>,
  variance?: Maybe<Todos_Variance_Fields>,
};


/** aggregate fields of "todos" */
export type Todos_Aggregate_FieldsCountArgs = {
  columns?: Maybe<Array<Todos_Select_Column>>,
  distinct?: Maybe<Scalars['Boolean']>
};

/** order by aggregate values of table "todos" */
export type Todos_Aggregate_Order_By = {
  avg?: Maybe<Todos_Avg_Order_By>,
  count?: Maybe<Order_By>,
  max?: Maybe<Todos_Max_Order_By>,
  min?: Maybe<Todos_Min_Order_By>,
  stddev?: Maybe<Todos_Stddev_Order_By>,
  stddev_pop?: Maybe<Todos_Stddev_Pop_Order_By>,
  stddev_samp?: Maybe<Todos_Stddev_Samp_Order_By>,
  sum?: Maybe<Todos_Sum_Order_By>,
  var_pop?: Maybe<Todos_Var_Pop_Order_By>,
  var_samp?: Maybe<Todos_Var_Samp_Order_By>,
  variance?: Maybe<Todos_Variance_Order_By>,
};

/** input type for inserting array relation for remote table "todos" */
export type Todos_Arr_Rel_Insert_Input = {
  data: Array<Todos_Insert_Input>,
  on_conflict?: Maybe<Todos_On_Conflict>,
};

/** aggregate avg on columns */
export type Todos_Avg_Fields = {
   __typename?: 'todos_avg_fields',
  id?: Maybe<Scalars['Float']>,
};

/** order by avg() on columns of table "todos" */
export type Todos_Avg_Order_By = {
  id?: Maybe<Order_By>,
};

/** Boolean expression to filter rows from the table "todos". All fields are combined with a logical 'AND'. */
export type Todos_Bool_Exp = {
  _and?: Maybe<Array<Maybe<Todos_Bool_Exp>>>,
  _not?: Maybe<Todos_Bool_Exp>,
  _or?: Maybe<Array<Maybe<Todos_Bool_Exp>>>,
  created_at?: Maybe<Timestamptz_Comparison_Exp>,
  id?: Maybe<Integer_Comparison_Exp>,
  is_completed?: Maybe<Boolean_Comparison_Exp>,
  is_public?: Maybe<Boolean_Comparison_Exp>,
  title?: Maybe<Text_Comparison_Exp>,
  user?: Maybe<Users_Bool_Exp>,
  user_id?: Maybe<Text_Comparison_Exp>,
};

/** unique or primary key constraints on table "todos" */
export enum Todos_Constraint {
  /** unique or primary key constraint */
  TodosPkey = 'todos_pkey'
}

/** input type for incrementing integer columne in table "todos" */
export type Todos_Inc_Input = {
  id?: Maybe<Scalars['Int']>,
};

/** input type for inserting data into table "todos" */
export type Todos_Insert_Input = {
  created_at?: Maybe<Scalars['timestamptz']>,
  id?: Maybe<Scalars['Int']>,
  is_completed?: Maybe<Scalars['Boolean']>,
  is_public?: Maybe<Scalars['Boolean']>,
  title?: Maybe<Scalars['String']>,
  user?: Maybe<Users_Obj_Rel_Insert_Input>,
  user_id?: Maybe<Scalars['String']>,
};

/** aggregate max on columns */
export type Todos_Max_Fields = {
   __typename?: 'todos_max_fields',
  created_at?: Maybe<Scalars['timestamptz']>,
  id?: Maybe<Scalars['Int']>,
  title?: Maybe<Scalars['String']>,
  user_id?: Maybe<Scalars['String']>,
};

/** order by max() on columns of table "todos" */
export type Todos_Max_Order_By = {
  created_at?: Maybe<Order_By>,
  id?: Maybe<Order_By>,
  title?: Maybe<Order_By>,
  user_id?: Maybe<Order_By>,
};

/** aggregate min on columns */
export type Todos_Min_Fields = {
   __typename?: 'todos_min_fields',
  created_at?: Maybe<Scalars['timestamptz']>,
  id?: Maybe<Scalars['Int']>,
  title?: Maybe<Scalars['String']>,
  user_id?: Maybe<Scalars['String']>,
};

/** order by min() on columns of table "todos" */
export type Todos_Min_Order_By = {
  created_at?: Maybe<Order_By>,
  id?: Maybe<Order_By>,
  title?: Maybe<Order_By>,
  user_id?: Maybe<Order_By>,
};

/** response of any mutation on the table "todos" */
export type Todos_Mutation_Response = {
   __typename?: 'todos_mutation_response',
  /** number of affected rows by the mutation */
  affected_rows: Scalars['Int'],
  /** data of the affected rows by the mutation */
  returning: Array<Todos>,
};

/** input type for inserting object relation for remote table "todos" */
export type Todos_Obj_Rel_Insert_Input = {
  data: Todos_Insert_Input,
  on_conflict?: Maybe<Todos_On_Conflict>,
};

/** on conflict condition type for table "todos" */
export type Todos_On_Conflict = {
  constraint: Todos_Constraint,
  update_columns: Array<Todos_Update_Column>,
};

/** ordering options when selecting data from "todos" */
export type Todos_Order_By = {
  created_at?: Maybe<Order_By>,
  id?: Maybe<Order_By>,
  is_completed?: Maybe<Order_By>,
  is_public?: Maybe<Order_By>,
  title?: Maybe<Order_By>,
  user?: Maybe<Users_Order_By>,
  user_id?: Maybe<Order_By>,
};

/** select columns of table "todos" */
export enum Todos_Select_Column {
  /** column name */
  CreatedAt = 'created_at',
  /** column name */
  Id = 'id',
  /** column name */
  IsCompleted = 'is_completed',
  /** column name */
  IsPublic = 'is_public',
  /** column name */
  Title = 'title',
  /** column name */
  UserId = 'user_id'
}

/** input type for updating data in table "todos" */
export type Todos_Set_Input = {
  created_at?: Maybe<Scalars['timestamptz']>,
  id?: Maybe<Scalars['Int']>,
  is_completed?: Maybe<Scalars['Boolean']>,
  is_public?: Maybe<Scalars['Boolean']>,
  title?: Maybe<Scalars['String']>,
  user_id?: Maybe<Scalars['String']>,
};

/** aggregate stddev on columns */
export type Todos_Stddev_Fields = {
   __typename?: 'todos_stddev_fields',
  id?: Maybe<Scalars['Float']>,
};

/** order by stddev() on columns of table "todos" */
export type Todos_Stddev_Order_By = {
  id?: Maybe<Order_By>,
};

/** aggregate stddev_pop on columns */
export type Todos_Stddev_Pop_Fields = {
   __typename?: 'todos_stddev_pop_fields',
  id?: Maybe<Scalars['Float']>,
};

/** order by stddev_pop() on columns of table "todos" */
export type Todos_Stddev_Pop_Order_By = {
  id?: Maybe<Order_By>,
};

/** aggregate stddev_samp on columns */
export type Todos_Stddev_Samp_Fields = {
   __typename?: 'todos_stddev_samp_fields',
  id?: Maybe<Scalars['Float']>,
};

/** order by stddev_samp() on columns of table "todos" */
export type Todos_Stddev_Samp_Order_By = {
  id?: Maybe<Order_By>,
};

/** aggregate sum on columns */
export type Todos_Sum_Fields = {
   __typename?: 'todos_sum_fields',
  id?: Maybe<Scalars['Int']>,
};

/** order by sum() on columns of table "todos" */
export type Todos_Sum_Order_By = {
  id?: Maybe<Order_By>,
};

/** update columns of table "todos" */
export enum Todos_Update_Column {
  /** column name */
  CreatedAt = 'created_at',
  /** column name */
  Id = 'id',
  /** column name */
  IsCompleted = 'is_completed',
  /** column name */
  IsPublic = 'is_public',
  /** column name */
  Title = 'title',
  /** column name */
  UserId = 'user_id'
}

/** aggregate var_pop on columns */
export type Todos_Var_Pop_Fields = {
   __typename?: 'todos_var_pop_fields',
  id?: Maybe<Scalars['Float']>,
};

/** order by var_pop() on columns of table "todos" */
export type Todos_Var_Pop_Order_By = {
  id?: Maybe<Order_By>,
};

/** aggregate var_samp on columns */
export type Todos_Var_Samp_Fields = {
   __typename?: 'todos_var_samp_fields',
  id?: Maybe<Scalars['Float']>,
};

/** order by var_samp() on columns of table "todos" */
export type Todos_Var_Samp_Order_By = {
  id?: Maybe<Order_By>,
};

/** aggregate variance on columns */
export type Todos_Variance_Fields = {
   __typename?: 'todos_variance_fields',
  id?: Maybe<Scalars['Float']>,
};

/** order by variance() on columns of table "todos" */
export type Todos_Variance_Order_By = {
  id?: Maybe<Order_By>,
};

/** columns and relationships of "users" */
export type Users = {
   __typename?: 'users',
  created_at: Scalars['timestamptz'],
  id: Scalars['String'],
  last_seen?: Maybe<Scalars['timestamptz']>,
  name: Scalars['String'],
  password?: Maybe<Scalars['String']>,
  /** An array relationship */
  todos: Array<Todos>,
  /** An aggregated array relationship */
  todos_aggregate: Todos_Aggregate,
};


/** columns and relationships of "users" */
export type UsersTodosArgs = {
  distinct_on?: Maybe<Array<Todos_Select_Column>>,
  limit?: Maybe<Scalars['Int']>,
  offset?: Maybe<Scalars['Int']>,
  order_by?: Maybe<Array<Todos_Order_By>>,
  where?: Maybe<Todos_Bool_Exp>
};


/** columns and relationships of "users" */
export type UsersTodos_AggregateArgs = {
  distinct_on?: Maybe<Array<Todos_Select_Column>>,
  limit?: Maybe<Scalars['Int']>,
  offset?: Maybe<Scalars['Int']>,
  order_by?: Maybe<Array<Todos_Order_By>>,
  where?: Maybe<Todos_Bool_Exp>
};

/** aggregated selection of "users" */
export type Users_Aggregate = {
   __typename?: 'users_aggregate',
  aggregate?: Maybe<Users_Aggregate_Fields>,
  nodes: Array<Users>,
};

/** aggregate fields of "users" */
export type Users_Aggregate_Fields = {
   __typename?: 'users_aggregate_fields',
  count?: Maybe<Scalars['Int']>,
  max?: Maybe<Users_Max_Fields>,
  min?: Maybe<Users_Min_Fields>,
};


/** aggregate fields of "users" */
export type Users_Aggregate_FieldsCountArgs = {
  columns?: Maybe<Array<Users_Select_Column>>,
  distinct?: Maybe<Scalars['Boolean']>
};

/** order by aggregate values of table "users" */
export type Users_Aggregate_Order_By = {
  count?: Maybe<Order_By>,
  max?: Maybe<Users_Max_Order_By>,
  min?: Maybe<Users_Min_Order_By>,
};

/** input type for inserting array relation for remote table "users" */
export type Users_Arr_Rel_Insert_Input = {
  data: Array<Users_Insert_Input>,
  on_conflict?: Maybe<Users_On_Conflict>,
};

/** Boolean expression to filter rows from the table "users". All fields are combined with a logical 'AND'. */
export type Users_Bool_Exp = {
  _and?: Maybe<Array<Maybe<Users_Bool_Exp>>>,
  _not?: Maybe<Users_Bool_Exp>,
  _or?: Maybe<Array<Maybe<Users_Bool_Exp>>>,
  created_at?: Maybe<Timestamptz_Comparison_Exp>,
  id?: Maybe<Text_Comparison_Exp>,
  last_seen?: Maybe<Timestamptz_Comparison_Exp>,
  name?: Maybe<Text_Comparison_Exp>,
  password?: Maybe<Text_Comparison_Exp>,
  todos?: Maybe<Todos_Bool_Exp>,
};

/** unique or primary key constraints on table "users" */
export enum Users_Constraint {
  /** unique or primary key constraint */
  UsersPkey = 'users_pkey'
}

/** input type for inserting data into table "users" */
export type Users_Insert_Input = {
  created_at?: Maybe<Scalars['timestamptz']>,
  id?: Maybe<Scalars['String']>,
  last_seen?: Maybe<Scalars['timestamptz']>,
  name?: Maybe<Scalars['String']>,
  password?: Maybe<Scalars['String']>,
  todos?: Maybe<Todos_Arr_Rel_Insert_Input>,
};

/** aggregate max on columns */
export type Users_Max_Fields = {
   __typename?: 'users_max_fields',
  created_at?: Maybe<Scalars['timestamptz']>,
  id?: Maybe<Scalars['String']>,
  last_seen?: Maybe<Scalars['timestamptz']>,
  name?: Maybe<Scalars['String']>,
  password?: Maybe<Scalars['String']>,
};

/** order by max() on columns of table "users" */
export type Users_Max_Order_By = {
  created_at?: Maybe<Order_By>,
  id?: Maybe<Order_By>,
  last_seen?: Maybe<Order_By>,
  name?: Maybe<Order_By>,
  password?: Maybe<Order_By>,
};

/** aggregate min on columns */
export type Users_Min_Fields = {
   __typename?: 'users_min_fields',
  created_at?: Maybe<Scalars['timestamptz']>,
  id?: Maybe<Scalars['String']>,
  last_seen?: Maybe<Scalars['timestamptz']>,
  name?: Maybe<Scalars['String']>,
  password?: Maybe<Scalars['String']>,
};

/** order by min() on columns of table "users" */
export type Users_Min_Order_By = {
  created_at?: Maybe<Order_By>,
  id?: Maybe<Order_By>,
  last_seen?: Maybe<Order_By>,
  name?: Maybe<Order_By>,
  password?: Maybe<Order_By>,
};

/** response of any mutation on the table "users" */
export type Users_Mutation_Response = {
   __typename?: 'users_mutation_response',
  /** number of affected rows by the mutation */
  affected_rows: Scalars['Int'],
  /** data of the affected rows by the mutation */
  returning: Array<Users>,
};

/** input type for inserting object relation for remote table "users" */
export type Users_Obj_Rel_Insert_Input = {
  data: Users_Insert_Input,
  on_conflict?: Maybe<Users_On_Conflict>,
};

/** on conflict condition type for table "users" */
export type Users_On_Conflict = {
  constraint: Users_Constraint,
  update_columns: Array<Users_Update_Column>,
};

/** ordering options when selecting data from "users" */
export type Users_Order_By = {
  created_at?: Maybe<Order_By>,
  id?: Maybe<Order_By>,
  last_seen?: Maybe<Order_By>,
  name?: Maybe<Order_By>,
  password?: Maybe<Order_By>,
  todos_aggregate?: Maybe<Todos_Aggregate_Order_By>,
};

/** select columns of table "users" */
export enum Users_Select_Column {
  /** column name */
  CreatedAt = 'created_at',
  /** column name */
  Id = 'id',
  /** column name */
  LastSeen = 'last_seen',
  /** column name */
  Name = 'name',
  /** column name */
  Password = 'password'
}

/** input type for updating data in table "users" */
export type Users_Set_Input = {
  created_at?: Maybe<Scalars['timestamptz']>,
  id?: Maybe<Scalars['String']>,
  last_seen?: Maybe<Scalars['timestamptz']>,
  name?: Maybe<Scalars['String']>,
  password?: Maybe<Scalars['String']>,
};

/** update columns of table "users" */
export enum Users_Update_Column {
  /** column name */
  CreatedAt = 'created_at',
  /** column name */
  Id = 'id',
  /** column name */
  LastSeen = 'last_seen',
  /** column name */
  Name = 'name',
  /** column name */
  Password = 'password'
}
export type UpdateLastSeenMutationVariables = {
  now: Scalars['timestamptz']
};


export type UpdateLastSeenMutation = (
  { __typename?: 'mutation_root' }
  & { update_users: Maybe<(
    { __typename?: 'users_mutation_response' }
    & Pick<Users_Mutation_Response, 'affected_rows'>
  )> }
);

export type GetOnlineUsersSubscriptionVariables = {};


export type GetOnlineUsersSubscription = (
  { __typename?: 'subscription_root' }
  & { online_users: Array<(
    { __typename?: 'online_users' }
    & Pick<Online_Users, 'id'>
    & { user: Maybe<(
      { __typename?: 'users' }
      & Pick<Users, 'name'>
    )> }
  )> }
);

export type Insert_TodosMutationVariables = {
  todo: Scalars['String'],
  isPublic: Scalars['Boolean']
};


export type Insert_TodosMutation = (
  { __typename?: 'mutation_root' }
  & { insert_todos: Maybe<(
    { __typename?: 'todos_mutation_response' }
    & Pick<Todos_Mutation_Response, 'affected_rows'>
    & { returning: Array<(
      { __typename?: 'todos' }
      & Pick<Todos, 'id' | 'title' | 'created_at' | 'is_public' | 'is_completed'>
    )> }
  )> }
);

export type ToggleTodoMutationVariables = {
  id: Scalars['Int'],
  isCompleted: Scalars['Boolean']
};


export type ToggleTodoMutation = (
  { __typename?: 'mutation_root' }
  & { update_todos: Maybe<(
    { __typename?: 'todos_mutation_response' }
    & Pick<Todos_Mutation_Response, 'affected_rows'>
  )> }
);

export type RemoveTodoMutationVariables = {
  id: Scalars['Int']
};


export type RemoveTodoMutation = (
  { __typename?: 'mutation_root' }
  & { delete_todos: Maybe<(
    { __typename?: 'todos_mutation_response' }
    & Pick<Todos_Mutation_Response, 'affected_rows'>
  )> }
);

export type GetMyTodosQueryVariables = {};


export type GetMyTodosQuery = (
  { __typename?: 'query_root' }
  & { todos: Array<(
    { __typename?: 'todos' }
    & Pick<Todos, 'id' | 'title' | 'created_at' | 'is_completed'>
  )> }
);

export type ClearCompletedMutationVariables = {};


export type ClearCompletedMutation = (
  { __typename?: 'mutation_root' }
  & { delete_todos: Maybe<(
    { __typename?: 'todos_mutation_response' }
    & Pick<Todos_Mutation_Response, 'affected_rows'>
  )> }
);

export type GetOldPublicTodosQueryVariables = {
  oldestTodoId: Scalars['Int']
};


export type GetOldPublicTodosQuery = (
  { __typename?: 'query_root' }
  & { todos: Array<(
    { __typename?: 'todos' }
    & Pick<Todos, 'id' | 'title' | 'created_at'>
    & { user: (
      { __typename?: 'users' }
      & Pick<Users, 'name'>
    ) }
  )> }
);

export type GetNewPublicTodosQueryVariables = {
  latestVisibleId?: Maybe<Scalars['Int']>
};


export type GetNewPublicTodosQuery = (
  { __typename?: 'query_root' }
  & { todos: Array<(
    { __typename?: 'todos' }
    & Pick<Todos, 'id' | 'title' | 'created_at'>
    & { user: (
      { __typename?: 'users' }
      & Pick<Users, 'name'>
    ) }
  )> }
);

export type NotifyNewPublicTodosSubscriptionVariables = {};


export type NotifyNewPublicTodosSubscription = (
  { __typename?: 'subscription_root' }
  & { todos: Array<(
    { __typename?: 'todos' }
    & Pick<Todos, 'id' | 'created_at'>
  )> }
);

export const UpdateLastSeenDocument = gql`
    mutation updateLastSeen($now: timestamptz!) {
  update_users(where: {}, _set: {last_seen: $now}) {
    affected_rows
  }
}
    `;
export type UpdateLastSeenMutationFn = ApolloReactCommon.MutationFunction<UpdateLastSeenMutation, UpdateLastSeenMutationVariables>;
export type UpdateLastSeenComponentProps = Omit<ApolloReactComponents.MutationComponentOptions<UpdateLastSeenMutation, UpdateLastSeenMutationVariables>, 'mutation'>;

    export const UpdateLastSeenComponent = (props: UpdateLastSeenComponentProps) => (
      <ApolloReactComponents.Mutation<UpdateLastSeenMutation, UpdateLastSeenMutationVariables> mutation={UpdateLastSeenDocument} {...props} />
    );
    
export type UpdateLastSeenProps<TChildProps = {}> = ApolloReactHoc.MutateProps<UpdateLastSeenMutation, UpdateLastSeenMutationVariables> & TChildProps;
export function withUpdateLastSeen<TProps, TChildProps = {}>(operationOptions?: ApolloReactHoc.OperationOption<
  TProps,
  UpdateLastSeenMutation,
  UpdateLastSeenMutationVariables,
  UpdateLastSeenProps<TChildProps>>) {
    return ApolloReactHoc.withMutation<TProps, UpdateLastSeenMutation, UpdateLastSeenMutationVariables, UpdateLastSeenProps<TChildProps>>(UpdateLastSeenDocument, {
      alias: 'updateLastSeen',
      ...operationOptions
    });
};
export type UpdateLastSeenMutationResult = ApolloReactCommon.MutationResult<UpdateLastSeenMutation>;
export type UpdateLastSeenMutationOptions = ApolloReactCommon.BaseMutationOptions<UpdateLastSeenMutation, UpdateLastSeenMutationVariables>;
export const GetOnlineUsersDocument = gql`
    subscription getOnlineUsers {
  online_users(order_by: {user: {name: asc}}) {
    id
    user {
      name
    }
  }
}
    `;
export type GetOnlineUsersComponentProps = Omit<ApolloReactComponents.SubscriptionComponentOptions<GetOnlineUsersSubscription, GetOnlineUsersSubscriptionVariables>, 'subscription'>;

    export const GetOnlineUsersComponent = (props: GetOnlineUsersComponentProps) => (
      <ApolloReactComponents.Subscription<GetOnlineUsersSubscription, GetOnlineUsersSubscriptionVariables> subscription={GetOnlineUsersDocument} {...props} />
    );
    
export type GetOnlineUsersProps<TChildProps = {}> = ApolloReactHoc.DataProps<GetOnlineUsersSubscription, GetOnlineUsersSubscriptionVariables> & TChildProps;
export function withGetOnlineUsers<TProps, TChildProps = {}>(operationOptions?: ApolloReactHoc.OperationOption<
  TProps,
  GetOnlineUsersSubscription,
  GetOnlineUsersSubscriptionVariables,
  GetOnlineUsersProps<TChildProps>>) {
    return ApolloReactHoc.withSubscription<TProps, GetOnlineUsersSubscription, GetOnlineUsersSubscriptionVariables, GetOnlineUsersProps<TChildProps>>(GetOnlineUsersDocument, {
      alias: 'getOnlineUsers',
      ...operationOptions
    });
};
export type GetOnlineUsersSubscriptionResult = ApolloReactCommon.SubscriptionResult<GetOnlineUsersSubscription>;
export const Insert_TodosDocument = gql`
    mutation insert_todos($todo: String!, $isPublic: Boolean!) {
  insert_todos(objects: {title: $todo, is_public: $isPublic}) {
    affected_rows
    returning {
      id
      title
      created_at
      is_public
      is_completed
    }
  }
}
    `;
export type Insert_TodosMutationFn = ApolloReactCommon.MutationFunction<Insert_TodosMutation, Insert_TodosMutationVariables>;
export type Insert_TodosComponentProps = Omit<ApolloReactComponents.MutationComponentOptions<Insert_TodosMutation, Insert_TodosMutationVariables>, 'mutation'>;

    export const Insert_TodosComponent = (props: Insert_TodosComponentProps) => (
      <ApolloReactComponents.Mutation<Insert_TodosMutation, Insert_TodosMutationVariables> mutation={Insert_TodosDocument} {...props} />
    );
    
export type Insert_TodosProps<TChildProps = {}> = ApolloReactHoc.MutateProps<Insert_TodosMutation, Insert_TodosMutationVariables> & TChildProps;
export function withInsert_Todos<TProps, TChildProps = {}>(operationOptions?: ApolloReactHoc.OperationOption<
  TProps,
  Insert_TodosMutation,
  Insert_TodosMutationVariables,
  Insert_TodosProps<TChildProps>>) {
    return ApolloReactHoc.withMutation<TProps, Insert_TodosMutation, Insert_TodosMutationVariables, Insert_TodosProps<TChildProps>>(Insert_TodosDocument, {
      alias: 'insertTodos',
      ...operationOptions
    });
};
export type Insert_TodosMutationResult = ApolloReactCommon.MutationResult<Insert_TodosMutation>;
export type Insert_TodosMutationOptions = ApolloReactCommon.BaseMutationOptions<Insert_TodosMutation, Insert_TodosMutationVariables>;
export const ToggleTodoDocument = gql`
    mutation toggleTodo($id: Int!, $isCompleted: Boolean!) {
  update_todos(where: {id: {_eq: $id}}, _set: {is_completed: $isCompleted}) {
    affected_rows
  }
}
    `;
export type ToggleTodoMutationFn = ApolloReactCommon.MutationFunction<ToggleTodoMutation, ToggleTodoMutationVariables>;
export type ToggleTodoComponentProps = Omit<ApolloReactComponents.MutationComponentOptions<ToggleTodoMutation, ToggleTodoMutationVariables>, 'mutation'>;

    export const ToggleTodoComponent = (props: ToggleTodoComponentProps) => (
      <ApolloReactComponents.Mutation<ToggleTodoMutation, ToggleTodoMutationVariables> mutation={ToggleTodoDocument} {...props} />
    );
    
export type ToggleTodoProps<TChildProps = {}> = ApolloReactHoc.MutateProps<ToggleTodoMutation, ToggleTodoMutationVariables> & TChildProps;
export function withToggleTodo<TProps, TChildProps = {}>(operationOptions?: ApolloReactHoc.OperationOption<
  TProps,
  ToggleTodoMutation,
  ToggleTodoMutationVariables,
  ToggleTodoProps<TChildProps>>) {
    return ApolloReactHoc.withMutation<TProps, ToggleTodoMutation, ToggleTodoMutationVariables, ToggleTodoProps<TChildProps>>(ToggleTodoDocument, {
      alias: 'toggleTodo',
      ...operationOptions
    });
};
export type ToggleTodoMutationResult = ApolloReactCommon.MutationResult<ToggleTodoMutation>;
export type ToggleTodoMutationOptions = ApolloReactCommon.BaseMutationOptions<ToggleTodoMutation, ToggleTodoMutationVariables>;
export const RemoveTodoDocument = gql`
    mutation removeTodo($id: Int!) {
  delete_todos(where: {id: {_eq: $id}}) {
    affected_rows
  }
}
    `;
export type RemoveTodoMutationFn = ApolloReactCommon.MutationFunction<RemoveTodoMutation, RemoveTodoMutationVariables>;
export type RemoveTodoComponentProps = Omit<ApolloReactComponents.MutationComponentOptions<RemoveTodoMutation, RemoveTodoMutationVariables>, 'mutation'>;

    export const RemoveTodoComponent = (props: RemoveTodoComponentProps) => (
      <ApolloReactComponents.Mutation<RemoveTodoMutation, RemoveTodoMutationVariables> mutation={RemoveTodoDocument} {...props} />
    );
    
export type RemoveTodoProps<TChildProps = {}> = ApolloReactHoc.MutateProps<RemoveTodoMutation, RemoveTodoMutationVariables> & TChildProps;
export function withRemoveTodo<TProps, TChildProps = {}>(operationOptions?: ApolloReactHoc.OperationOption<
  TProps,
  RemoveTodoMutation,
  RemoveTodoMutationVariables,
  RemoveTodoProps<TChildProps>>) {
    return ApolloReactHoc.withMutation<TProps, RemoveTodoMutation, RemoveTodoMutationVariables, RemoveTodoProps<TChildProps>>(RemoveTodoDocument, {
      alias: 'removeTodo',
      ...operationOptions
    });
};
export type RemoveTodoMutationResult = ApolloReactCommon.MutationResult<RemoveTodoMutation>;
export type RemoveTodoMutationOptions = ApolloReactCommon.BaseMutationOptions<RemoveTodoMutation, RemoveTodoMutationVariables>;
export const GetMyTodosDocument = gql`
    query getMyTodos {
  todos(where: {is_public: {_eq: false}}, order_by: {created_at: desc}) {
    id
    title
    created_at
    is_completed
  }
}
    `;
export type GetMyTodosComponentProps = Omit<ApolloReactComponents.QueryComponentOptions<GetMyTodosQuery, GetMyTodosQueryVariables>, 'query'>;

    export const GetMyTodosComponent = (props: GetMyTodosComponentProps) => (
      <ApolloReactComponents.Query<GetMyTodosQuery, GetMyTodosQueryVariables> query={GetMyTodosDocument} {...props} />
    );
    
export type GetMyTodosProps<TChildProps = {}> = ApolloReactHoc.DataProps<GetMyTodosQuery, GetMyTodosQueryVariables> & TChildProps;
export function withGetMyTodos<TProps, TChildProps = {}>(operationOptions?: ApolloReactHoc.OperationOption<
  TProps,
  GetMyTodosQuery,
  GetMyTodosQueryVariables,
  GetMyTodosProps<TChildProps>>) {
    return ApolloReactHoc.withQuery<TProps, GetMyTodosQuery, GetMyTodosQueryVariables, GetMyTodosProps<TChildProps>>(GetMyTodosDocument, {
      alias: 'getMyTodos',
      ...operationOptions
    });
};
export type GetMyTodosQueryResult = ApolloReactCommon.QueryResult<GetMyTodosQuery, GetMyTodosQueryVariables>;
export const ClearCompletedDocument = gql`
    mutation clearCompleted {
  delete_todos(where: {is_completed: {_eq: true}, is_public: {_eq: false}}) {
    affected_rows
  }
}
    `;
export type ClearCompletedMutationFn = ApolloReactCommon.MutationFunction<ClearCompletedMutation, ClearCompletedMutationVariables>;
export type ClearCompletedComponentProps = Omit<ApolloReactComponents.MutationComponentOptions<ClearCompletedMutation, ClearCompletedMutationVariables>, 'mutation'>;

    export const ClearCompletedComponent = (props: ClearCompletedComponentProps) => (
      <ApolloReactComponents.Mutation<ClearCompletedMutation, ClearCompletedMutationVariables> mutation={ClearCompletedDocument} {...props} />
    );
    
export type ClearCompletedProps<TChildProps = {}> = ApolloReactHoc.MutateProps<ClearCompletedMutation, ClearCompletedMutationVariables> & TChildProps;
export function withClearCompleted<TProps, TChildProps = {}>(operationOptions?: ApolloReactHoc.OperationOption<
  TProps,
  ClearCompletedMutation,
  ClearCompletedMutationVariables,
  ClearCompletedProps<TChildProps>>) {
    return ApolloReactHoc.withMutation<TProps, ClearCompletedMutation, ClearCompletedMutationVariables, ClearCompletedProps<TChildProps>>(ClearCompletedDocument, {
      alias: 'clearCompleted',
      ...operationOptions
    });
};
export type ClearCompletedMutationResult = ApolloReactCommon.MutationResult<ClearCompletedMutation>;
export type ClearCompletedMutationOptions = ApolloReactCommon.BaseMutationOptions<ClearCompletedMutation, ClearCompletedMutationVariables>;
export const GetOldPublicTodosDocument = gql`
    query getOldPublicTodos($oldestTodoId: Int!) {
  todos(where: {is_public: {_eq: true}, id: {_lt: $oldestTodoId}}, limit: 7, order_by: {created_at: desc}) {
    id
    title
    created_at
    user {
      name
    }
  }
}
    `;
export type GetOldPublicTodosComponentProps = Omit<ApolloReactComponents.QueryComponentOptions<GetOldPublicTodosQuery, GetOldPublicTodosQueryVariables>, 'query'> & ({ variables: GetOldPublicTodosQueryVariables; skip?: boolean; } | { skip: boolean; });

    export const GetOldPublicTodosComponent = (props: GetOldPublicTodosComponentProps) => (
      <ApolloReactComponents.Query<GetOldPublicTodosQuery, GetOldPublicTodosQueryVariables> query={GetOldPublicTodosDocument} {...props} />
    );
    
export type GetOldPublicTodosProps<TChildProps = {}> = ApolloReactHoc.DataProps<GetOldPublicTodosQuery, GetOldPublicTodosQueryVariables> & TChildProps;
export function withGetOldPublicTodos<TProps, TChildProps = {}>(operationOptions?: ApolloReactHoc.OperationOption<
  TProps,
  GetOldPublicTodosQuery,
  GetOldPublicTodosQueryVariables,
  GetOldPublicTodosProps<TChildProps>>) {
    return ApolloReactHoc.withQuery<TProps, GetOldPublicTodosQuery, GetOldPublicTodosQueryVariables, GetOldPublicTodosProps<TChildProps>>(GetOldPublicTodosDocument, {
      alias: 'getOldPublicTodos',
      ...operationOptions
    });
};
export type GetOldPublicTodosQueryResult = ApolloReactCommon.QueryResult<GetOldPublicTodosQuery, GetOldPublicTodosQueryVariables>;
export const GetNewPublicTodosDocument = gql`
    query getNewPublicTodos($latestVisibleId: Int) {
  todos(where: {is_public: {_eq: true}, id: {_gt: $latestVisibleId}}, order_by: {created_at: desc}) {
    id
    title
    created_at
    user {
      name
    }
  }
}
    `;
export type GetNewPublicTodosComponentProps = Omit<ApolloReactComponents.QueryComponentOptions<GetNewPublicTodosQuery, GetNewPublicTodosQueryVariables>, 'query'>;

    export const GetNewPublicTodosComponent = (props: GetNewPublicTodosComponentProps) => (
      <ApolloReactComponents.Query<GetNewPublicTodosQuery, GetNewPublicTodosQueryVariables> query={GetNewPublicTodosDocument} {...props} />
    );
    
export type GetNewPublicTodosProps<TChildProps = {}> = ApolloReactHoc.DataProps<GetNewPublicTodosQuery, GetNewPublicTodosQueryVariables> & TChildProps;
export function withGetNewPublicTodos<TProps, TChildProps = {}>(operationOptions?: ApolloReactHoc.OperationOption<
  TProps,
  GetNewPublicTodosQuery,
  GetNewPublicTodosQueryVariables,
  GetNewPublicTodosProps<TChildProps>>) {
    return ApolloReactHoc.withQuery<TProps, GetNewPublicTodosQuery, GetNewPublicTodosQueryVariables, GetNewPublicTodosProps<TChildProps>>(GetNewPublicTodosDocument, {
      alias: 'getNewPublicTodos',
      ...operationOptions
    });
};
export type GetNewPublicTodosQueryResult = ApolloReactCommon.QueryResult<GetNewPublicTodosQuery, GetNewPublicTodosQueryVariables>;
export const NotifyNewPublicTodosDocument = gql`
    subscription notifyNewPublicTodos {
  todos(where: {is_public: {_eq: true}}, limit: 1, order_by: {created_at: desc}) {
    id
    created_at
  }
}
    `;
export type NotifyNewPublicTodosComponentProps = Omit<ApolloReactComponents.SubscriptionComponentOptions<NotifyNewPublicTodosSubscription, NotifyNewPublicTodosSubscriptionVariables>, 'subscription'>;

    export const NotifyNewPublicTodosComponent = (props: NotifyNewPublicTodosComponentProps) => (
      <ApolloReactComponents.Subscription<NotifyNewPublicTodosSubscription, NotifyNewPublicTodosSubscriptionVariables> subscription={NotifyNewPublicTodosDocument} {...props} />
    );
    
export type NotifyNewPublicTodosProps<TChildProps = {}> = ApolloReactHoc.DataProps<NotifyNewPublicTodosSubscription, NotifyNewPublicTodosSubscriptionVariables> & TChildProps;
export function withNotifyNewPublicTodos<TProps, TChildProps = {}>(operationOptions?: ApolloReactHoc.OperationOption<
  TProps,
  NotifyNewPublicTodosSubscription,
  NotifyNewPublicTodosSubscriptionVariables,
  NotifyNewPublicTodosProps<TChildProps>>) {
    return ApolloReactHoc.withSubscription<TProps, NotifyNewPublicTodosSubscription, NotifyNewPublicTodosSubscriptionVariables, NotifyNewPublicTodosProps<TChildProps>>(NotifyNewPublicTodosDocument, {
      alias: 'notifyNewPublicTodos',
      ...operationOptions
    });
};
export type NotifyNewPublicTodosSubscriptionResult = ApolloReactCommon.SubscriptionResult<NotifyNewPublicTodosSubscription>;