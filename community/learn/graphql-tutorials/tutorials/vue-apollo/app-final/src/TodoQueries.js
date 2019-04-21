import gql from "graphql-tag";

const TODO_FRAGMENT = gql`
  fragment TodoFragment on todos {
    id
    title
    is_completed
    created_at
    is_public
  }
`;

const USER_FRAGMENT = gql`
  fragment UserFragment on users {
    name
  }
`;

const GET_MY_TODOS = gql`
  query getMyTodos {
    todos(
      where: { is_public: { _eq: false } }
      order_by: { created_at: desc }
    ) {
      ...TodoFragment
    }
  }
  ${TODO_FRAGMENT}
`;

const GET_NEW_PUBLIC_TODOS = gql`
  query getNewPublicTodos($latestVisibleId: Int!) {
    todos(
      where: { is_public: { _eq: true }, id: { _gt: $latestVisibleId } }
      order_by: { created_at: desc }
    ) {
      ...TodoFragment
      user {
        ...UserFragment
      }
    }
  }
  ${TODO_FRAGMENT}
  ${USER_FRAGMENT}
`;

const GET_OLD_PUBLIC_TODOS = gql`
  query getOldPublicTodos($oldestTodoId: Int) {
    todos(
      where: { is_public: { _eq: true }, id: { _lt: $oldestTodoId } }
      limit: 7
      order_by: { created_at: desc }
    ) {
      ...TodoFragment
      user {
        ...UserFragment
      }
    }
  }
  ${TODO_FRAGMENT}
  ${USER_FRAGMENT}
`;

const ADD_TODO = gql`
  mutation insert_todos($todo: String!, $isPublic: Boolean!) {
    insert_todos(objects: {title: $todo, is_public: $isPublic}) {
      affected_rows
      returning {
        ...TodoFragment
      }
    }
  }
  ${TODO_FRAGMENT}
`;

const TOGGLE_TODO = gql`
  mutation update_todos($id: Int!, $isCompleted: Boolean!) {
    update_todos(where: { id: { _eq: $id } }, _set: { is_completed: $isCompleted }) {
      affected_rows
    }
  }
`;

const REMOVE_TODO = gql`
  mutation delete_todos($id: Int!) {
    delete_todos(where: { id: { _eq: $id } }) {
      affected_rows
    }
  }
`;

const NOTIFY_NEW_PUBLIC_TODOS = gql`
  subscription notifyNewPublicTodos {
    todos(
      where: { is_public: { _eq: true }} 
      order_by: { created_at: desc }
      limit: 1
    ) {
      ...TodoFragment
    }
  }
  ${TODO_FRAGMENT}
`;

export {
  GET_MY_TODOS,
  GET_NEW_PUBLIC_TODOS,
  GET_OLD_PUBLIC_TODOS,
  ADD_TODO,
  TOGGLE_TODO,
  REMOVE_TODO,
  NOTIFY_NEW_PUBLIC_TODOS
};
