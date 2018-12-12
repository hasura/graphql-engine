import gql from "graphql-tag";

export const FetchAllQuery = gql`
  {
    todos (
      where: { completed: { _eq: false }},
      order_by: id_desc
    ) {
      id
      todo_name
      completed
      user_id
    }
  }
`;

export const AddTodoQuery = gql`
  mutation addTodo($todo_name: String!, $user_id: String!) {
    insert_todos(objects: [
      {
        todo_name: $todo_name,
        user_id: $user_id
      }
    ]) {
      returning {
        id
        todo_name
        completed
        user_id
      }
    }
  }
`;

export const MarkCompletedQuery = gql`
  mutation mark($id: Int!) {
    update_todos(
      where: {id: { _eq: $id }},
      _set: { completed: true }
    ) {
      affected_rows
    }
  }
`;

export const getCompletedQuery = gql`
  {
    todos(
      where: { completed: {_eq: true }},
      order_by: id_desc
    ) {
      id
      todo_name
      completed
      user_id
    }
  }
`;

export const deleteQuery = gql`
  mutation del($id: Int!) {
    delete_todos(
      where: {id: { _eq: $id }}
    ) {
      affected_rows
    }
  }
`;
