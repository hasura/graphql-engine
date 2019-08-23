import gql from 'graphql-tag';

const QUERY_TODO = gql`
  query fetch_todos {
    todo {
      id
      task
      completed
    }
  }
`;

const MUTATION_TODO_ADD = gql`
  mutation insert_todo ($objects: [todo_insert_input!]){
    insert_todo(objects: $objects) {
      affected_rows
      returning {
        id
        task
        completed
      }
    }
  }
`;

const MUTATION_TODO_UPDATE = gql`
  mutation update_todo ($todoId: Int, $set: todo_set_input!) {
    update_todo(where: {id: {_eq: $todoId}} _set: $set) {
      affected_rows
    }
  }
`;

const MUTATION_TODO_DELETE = gql`
  mutation delete_todo ($todoId: Int) {
    delete_todo(where: {id: {_eq: $todoId}}) {
      affected_rows
    }
  }
`;

export {
  QUERY_TODO,
  MUTATION_TODO_ADD,
  MUTATION_TODO_UPDATE,
  MUTATION_TODO_DELETE
};
