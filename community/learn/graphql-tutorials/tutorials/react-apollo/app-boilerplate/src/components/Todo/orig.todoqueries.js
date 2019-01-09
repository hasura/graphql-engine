import gql from "graphql-tag";

const QUERY_PRIVATE_TODO = gql`
  query fetch_todos($userId: String!) {
    todos(
      where: { is_public: { _eq: false }, user_id: { _eq: $userId } }
      order_by: { created_at: desc }
    ) {
      id
      text
      is_completed
      created_at
      is_public
    }
  }
`;

const QUERY_PUBLIC_TODO = gql`
  query fetch_todos {
    todos(where: { is_public: { _eq: true } }, order_by: { created_at: desc }) {
      id
      text
      is_completed
      created_at
      is_public
    }
  }
`;

const MUTATION_TODO_ADD = gql`
  mutation insert_todos($objects: [todos_insert_input!]) {
    insert_todos(objects: $objects) {
      affected_rows
      returning {
        id
        text
        is_completed
        created_at
        is_public
      }
    }
  }
`;

const MUTATION_TODO_UPDATE = gql`
  mutation update_todos($todoId: Int, $set: todos_set_input!) {
    update_todos(where: { id: { _eq: $todoId } }, _set: $set) {
      affected_rows
    }
  }
`;

const MUTATION_TODO_DELETE = gql`
  mutation delete_todos($todoId: Int) {
    delete_todos(where: { id: { _eq: $todoId } }) {
      affected_rows
    }
  }
`;

const SUBSCRIPTION_TODO_PUBLIC_LIST = gql`
  subscription {
    todos(where: { is_public: { _eq: true } }, order_by: { created_at: desc }) {
      id
      text
      is_completed
      created_at
      is_public
    }
  }
`;

const SUBSCRIPTION_ONLINE_USERS = gql`
  subscription {
    online_users(order_by: { name: asc }) {
      name
    }
  }
`;

export {
  QUERY_PRIVATE_TODO,
  QUERY_PUBLIC_TODO,
  MUTATION_TODO_ADD,
  MUTATION_TODO_UPDATE,
  MUTATION_TODO_DELETE,
  SUBSCRIPTION_TODO_PUBLIC_LIST,
  SUBSCRIPTION_ONLINE_USERS
};
