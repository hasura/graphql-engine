# Angular-Hasura-Boilerplate - basic

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

This starts the development server at localhost:4200.

### About this boilerplate

This boilerplate is build upon the `hello-world` boilerplate which contained the basic set up for the Apollo Client.

In this boilerplate we'll be getting around the basic operations which can be performed on a table. Operations like **query** **mutation** and **subscription** will be used in this example.

We'll be using a deployed app of Hasura GraphQL Engine on heroku. To setup the app [follow this link](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku) to goto Heroku and setup the GraphQL Engine.

#### Schema :

We'll be using following schema for the table on the deployed Hasura application.

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

Let's dive into the operations for the application.

```
  All the operations related to the applications are defined at
```

#### Query

The following theory gets the fields `id` `text` `is_completed` `created_at` `updated_at` `is_public` and `user_id` from the table called `todos` in the Heroku deployed Hasura engine.

```
  query GetQuery {
    todos {
      id
      text
      is_completed
      created_at
      updated_at
      is_public
      user_id
    }
  }
```

#### Mutation

###### 1. `AddMutation` or Insert Operation

The following mutation inserts the object provided by `$objects` variable in the table called `todos` and returns the items specified in the `returning` portion of the mutation.

```
  mutation AddMutation($objects: [todos_insert_input!]!) {
    insert_todos(objects: $objects) {
      returning {
        id
        text
        is_completed
        created_at
        updated_at
        is_public
        user_id
      }
    }
  }
```

This mutation adds user in the table called `users` if it not already exists.

```
  mutation AddUserMutation($objects: [users_insert_input!]!) {
    insert_users(objects: $objects) {
      returning {
        id
        name
        created_at
        last_seen
      }
    }
  }
```

###### 2. `UpdateMutation` or Update Operation

This mutation takes care of the updating of fields in the `todos` table. The variable `$where` takes the condition that needs to be fullfilled to make the required change and the variable `$set` take the vaalue that will be changed.

```
  mutation UpdateMutation($where: todos_bool_exp!, $set: todos_set_input!) {
    update_todos(where: $where, _set: $set) {
      affected_rows
      returning {
        id
        text
        is_completed
        created_at
        updated_at
        is_public
        user_id
      }
    }
  }
```

###### 3. `DeleteMutation` or Delete Operation

The delete mutation takes the condition from the `$where` variable and uses it to find the row that needs to be deleted.

```
  mutation DeleteMutation($where: todos_bool_exp!) {
    delete_todos(where: $where) {
      affected_rows
      returning {
        id
      }
    }
  }
```

The App can be deployed immidiately on `heroku` with no further setup or configurations required.

### Screenshots

##### Login Screen :

![login](/basic/ss/login.png)

#

#### Dashboard :

![dash](/basic/ss/dashboard.png)
