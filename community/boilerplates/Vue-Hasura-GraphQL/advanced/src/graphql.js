import gql from 'graphql-tag'

export const ADD_TODO = gql`
  mutation addTodo($desc: String!, $userid: String!) {
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
  subscription todosQuery {
    todos(
      where: { is_completed: { _eq: false }}
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
  subscription todosQuery {
    todos(
      where: { is_completed: { _eq: true }}
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
