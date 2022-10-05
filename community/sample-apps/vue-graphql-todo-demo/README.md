# vue-graphql-demo

## Project setup
```
npm install
```

## Secrets


Remember to get your URL and x-hasura-admin-secret from the GraphQL/API tab and replace it in apollo.js
```
uri: 'YOUR URI',
     headers: {
          "x-hasura-admin-secret": "YOUR API KEY"
        }
```

### Compiles and hot-reloads for development
```
npm run serve
```

### Compiles and minifies for production
```
npm run build
```

### Lints and fixes files
```
npm run lint
```

### Customize configuration
See [Configuration Reference](https://cli.vuejs.org/config/).



## CRUD Operations

### CREATE

```

const name = this.name;
const email = this.email;
this.$apollo.mutate({
mutation: gql`
mutation addUser($name: String!, $email: String!) {
        insert_users(objects: [{ name: $name, email: $email }]) {
             returning {
               user_id
             }
         }
       }
`,
variables: {
   name,
   Email,
},
refetchQueries: ["getUsers"],
});
```

### READ

```

apollo: {
   userList: {
     query: gql`
       query getUsers {
         users {
           user_id
           name
           email
         }
       }
     `,
     update: (data) => {
       return data.users;
     },
   },
 },

```

### UPDATE

```

const name = this.editUser.name;
const email = this.editUser.email;
const user_id = this.editUser.user_id;
this.$apollo.mutate({
   mutation: gql`
    mutation updateUsers($user_id: Int, $name: String!, $email: String!) {
      update_users(
        where: { user_id: { _eq: $user_id } }
          _set: { email: $email, name: $name }
        ) {
           returning {
           user_id
          }
        }	
      }
    `,
      variables: {
        user_id,
        name,
        email,
     },
      refetchQueries: ["getUsers"],
   });
```

### DELETE

```

let user_id = id;
   this.$apollo.mutate({
     mutation: gql`
       mutation deleteUser($user_id: Int) {
        delete_users(where: { user_id: { _eq: $user_id } }) {
           returning {
             user_id
           }
         }
       }
      `,
     variables: {
        user_id,
     },
     refetchQueries: ["getUsers"],
  });
```