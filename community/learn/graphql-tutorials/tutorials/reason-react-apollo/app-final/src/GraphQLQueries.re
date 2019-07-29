// GraphQL query for getting my todos
module GetMyTodos = [%graphql
  {|
    query getMyTodos {
      todos(where: { is_public: { _eq: false} }, order_by: { id: desc }) {
        id
        title
        is_completed
        is_public
      }
    }
  |}
];
module GetMyTodosQuery = ReasonApollo.CreateQuery(GetMyTodos);

// GraphQL mutation for inserting a todo
module InsertMyTodo = [%graphql
  {|
    mutation ($todo: String!, $isPublic: Boolean!) {
      insert_todos(objects: {title: $todo, is_public: $isPublic}) {
        affected_rows
        returning {
          id
          title
          created_at
          is_completed
        }
      }
    }
  |}
];
module InsertMyTodoMutation = ReasonApollo.CreateMutation(InsertMyTodo);

// GraphQL mutation for toggling a todo
module ToggleMyTodo = [%graphql
  {|
    mutation toggleTodo ($id: Int!, $isCompleted: Boolean!) {
      update_todos(where: {id: {_eq: $id}}, _set: {is_completed: $isCompleted}) {
        returning {
          id
          title
          created_at
          is_completed
        }
      }
    }
  |}
];
module ToggleMyTodoMutation = ReasonApollo.CreateMutation(ToggleMyTodo);

// GraphQL mutation for removing a todo
module DeleteMyTodo = [%graphql
  {|
    mutation removeTodo ($id: Int!) {
      delete_todos(where: {id: {_eq: $id}}) {
        affected_rows
      }
    }
  |}
];
module DeleteMyTodoMutation = ReasonApollo.CreateMutation(DeleteMyTodo);

// GraphQL subscription to get the list of online users
module GetOnlineUsers = [%graphql
  {|
    subscription getOnlineUsers {
      online_users(order_by: {user: {name: asc }}) {
        id
        user {
          name
        }
      }
    }
  |}
];
module GetOnlineUsersSubscription = ReasonApollo.CreateSubscription(GetOnlineUsers);

// GraphQL mutation to update last seen
module UpdateLastSeen = [%graphql
  {|
    mutation updateLastSeen {
      update_users(where: {}, _set: {last_seen: "now()"}) {
        affected_rows
      }
    }
  |}
];

// GraphQL query to fetch public todos
module GetPublicTodos = [%graphql
  {|
     query getPublicTodos {
       todos(where: { is_public: { _eq: true } }, order_by: { id: desc }, limit: 10) {
        id
         title
         created_at
         user {
           name
         }
       }
     }
  |}
];

// GraphQL query to public todos older than a particular id
module GetOlderTodos = [%graphql
  {|
    query ($lastId: Int) {
      todos (
        order_by: {
         created_at: desc
        },
        where: { id: { _lt: $lastId} },
        limit: 10
      ) {
        id
        title
        created_at
        user {
          name
        }
      }
    }
  |}
];

module NotifyNewPublicTodos = [%graphql
  {|
    subscription notifyNewPublicTodos {
      todos (where: { is_public: { _eq: true}}, limit: 1, order_by: {id: desc }) {
        id
      }
    }
  |}
];

module NotifyNewPublicTodosSubscription = ReasonApollo.CreateSubscription(NotifyNewPublicTodos);

module GetNewPublicTodos = [%graphql
  {|
    query getNewPublicTodos ($latestVisibleId: Int!) {
      todos(where: { is_public: { _eq: true}, id: {_gt: $latestVisibleId}}, order_by: { id: desc }) {
        id
        title
        user {
          name
        }
      }
    }
  |}
];
