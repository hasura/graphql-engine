# React-Hasura-Boilerplate - basic

## Getting Started

### Installing

You need to first install all the npm packages that are used in this boilerplate.

```
$ npm install
```

Once all the dependencies are installed, you're ready to go!

```
$ npm start
```
This starts the development server at port 3000 (localhost).


### What does this boilerplate contain ?

This boilerplate is built on top of the `hello-world` boilerplate so the initial `ApolloClient` setup is already done!

This boilerplate aims to help you with writing your __query/mutation/subscription__ operations and how to perform them.

The App is a simple __todo__ application with a basic authentication system which works as follows:

- user enter their name, the name is parallely checked for it's availability, if available a __green__ tick is shown.
- if a __green__ tick is not shown, which means the user already exits, therefore a login button is shown instead and the user is forwarded to their respective dashboard.
- The user can then `add todos`, `mark them completed` and `delete todos`.

Below shown are some code snippets of the __query/mutation/subscription__ used in the following app :

#### Schema :

```
todo:
id - (integer auto-increment) primary key  
text  - (text)
is_completed  - (bool)
created_at  - (timestamp) default - now()
updated_at  - (timestamp) nullable
is_public  - (bool) default - false
user_id - foreign key to users table

users:
id - (integer auto-increment) primary key
name - text
created_at - timestamp default - now()
last_seen - timestamp
```

######  __( all queries present in `/src/Queries/queries.js` )__
---
#### Query :

###### 1. `checkNameExistQuery` :
This query takes in a variable `name` and checks in the `users` table for an entry with the passed `name`.
#
```
query checkName($name: String!) {
    users (
      where: { name: {_eq: $name }},
    ) {
      id
      name
      created_at
      last_seen
    }
}
```
###### 2. `fetchUserQuery` :
This query just fetches the name of the user after login so that it is displayed on the navbar in the dashboard page.
#
```
query fetchName($name: String!) {
    users (
      where: { name: {_eq: $name }},
    ) {
      name
    }
}
```

#### Mutation :

###### 1. `addTodoQuery` :  [ `insert` ]
Simply adds a todo into the table `todo`, arguments passed into the mutation functions are the `data` - information in the todo & `user_id` - the id of the user that added the todo.
#
```
mutation addTodo($data: String!, $user_id: Int!) {
    insert_todo (
      objects: [
        {
          data: $data,
          user_id: $user_id
        }
      ]
    ) {
      returning {
        id
        data
        is_completed
        created_at
        updated_at
        is_public
      }
    }
}
```

###### 2. `markCompletedQuery` :  [ `update` ]
When the user clicks on the todo, it is striked through showing it is completed setting the value of `is_completed` in the `todo` table as __true__ also the timestamp is passed into the mutation function storing the time when the todo was completed.
#
```
mutation completeTodo($id: Int!, $updated_at: timestamptz!) {
    update_todo (
      where: { id: { _eq: $id }},
      _set: { is_completed: true, updated_at: $updated_at }
    ) {
      affected_rows
    }
}
```

###### 3. `deleteQuery` :  [ `delete` ]
Takes in the `id` of the todo that needs to be deleted and performs a delete operations on that todo causing the entry to be deleted from `todo` table.
#
```
mutation deleteTodo($id: Int!) {
    delete_todo(
      where: {id: { _eq: $id }}
    ) {
      affected_rows
    }
}
```
#### Subcription :

###### 1. `fetchTodosUncompletedSubs` :
This subcription is fired everytime a new todo is added to the `todo` table.
#
```
subscription fetchTodos($user_id: Int!) {
    todo (
      where: { user_id: {_eq: $user_id }, is_completed: { _eq: false }},
      order_by: id_desc
    ) {
      id
      data
      is_completed
      created_at
      updated_at
      is_public
    }
}
```

###### 2. `fetchTodosCompletedSubs` :
This subcription is fired everytime a todo is marked completed.
#
```
subscription fetchTodosCompleted($user_id: Int!) {
    todo (
      where: { user_id: {_eq: $user_id }, is_completed: { _eq: true }},
      order_by: id_desc
    ) {
      id
      data
      is_completed
      created_at
      updated_at
      is_public
    }
}
```

### Deployment

First create a file `env.js` in your `/src` folder, this file will contain your environment variables needed for the app to work.

```
export const vars = {
  "GRAPHQL_ENDPOINT": "https://",
  "GRAPHQL_SUBS_ENDPOINT": "ws://",
}
```
The App can be deployed immidiately on `heroku` with no further setup or configurations required.

### Screenshots

##### Login Screen :

![login](/basic/ss/login.png)

#
#### Dashboard :

![dash](/basic/ss/dash.png)
