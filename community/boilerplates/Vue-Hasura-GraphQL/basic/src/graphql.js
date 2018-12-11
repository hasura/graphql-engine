import gql from 'graphql-tag'

export const ADD_USER = gql`
  mutation addUser($username: String!) {
    insert_users(
      objects:[{
        name: $username
      }]
    ) {
      returning {
        id
        name
      }
    }
  }
`

export const ADD_TODO = gql`
  mutation addUser($desc: String!, $userid: Int!) {
    insert_todos(
      objects:[{
        text: $desc
        user_id: $userid
      }]
    ) {
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
`

export const ALL_PENDING_TODOS = gql`
  subscription todosQuery($userid: Int!) {
    todos(
      where: { user_id: {_eq: $userid}, is_completed: { _eq: false }}
      order_by: id_desc
    ) {
      id
      text
      is_completed
      created_at
      updated_at
      is_public
      user_id
    }
  }
`

export const ALL_COMPLETED_TODOS = gql`
  subscription todosQuery($userid: Int!) {
    todos(
      where: { user_id: {_eq: $userid}, is_completed: { _eq: true }}
      order_by: id_desc
    ) {
      id
      text
      is_completed
      created_at
      updated_at
      is_public
      user_id
    }
  }
`

export const GET_USER_BY_ID = gql`
  query userQuery($userid: Int!) {
    users(
      where: { id: {_eq: $userid}}
    ) {
      id
      name
    }
  }
`
export const GET_USER_BY_NAME = gql`
  query userQuery($name: String!) {
    users(
      where: { name: {_eq: $name}}
    ) {
      id
      name
    }
  }
`

export const UPDATE_TODO = gql`
  mutation updateTodo($id: Int!, $updated: timestamptz!) {
    update_todos(
      where: { id: {_eq: $id}},
      _set: { is_completed: true, updated_at: $updated }
    ) {
      affected_rows
    }
  }
`

export const DELETE_TODO = gql`
  mutation deleteTodo($id: Int! ) {
    delete_todos(
      where: { id: {_eq: $id}},
    ) {
      affected_rows
    }
  }
`
