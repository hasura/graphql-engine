import gql from "graphql-tag";

//query
export const checkNameExistQuery = gql`
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
`;

export const fetchUserQuery = gql`
  query fetchName($name: String!) {
    users (
      where: { name: {_eq: $name }},
    ) {
      name
    }
  }
`;

//mutation
export const addNameQuery = gql`
  mutation addname($name: String!) {
    insert_users(
      objects: [
        {
          name: $name,
        }
      ]
    ) {
      returning {
        id
        name
        created_at
        last_seen
      }
    }
  }
`;

export const addTodoQuery = gql`
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
`;

export const markCompletedQuery = gql`
  mutation completeTodo($id: Int!, $updated_at: timestamptz!) {
    update_todo (
      where: { id: { _eq: $id }},
      _set: { is_completed: true, updated_at: $updated_at }
    ) {
      affected_rows
    }
  }
`;

export const deleteQuery = gql`
  mutation deleteTodo($id: Int!) {
    delete_todo(
      where: {id: { _eq: $id }}
    ) {
      affected_rows
    }
  }
`;

//subscription
export const fetchTodosUncompletedSubs = gql`
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
`;

export const fetchTodosCompletedSubs = gql`
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
`;
