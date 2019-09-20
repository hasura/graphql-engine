import * as React from 'react';
import gql from 'graphql-tag';
import { useMutation } from '@apollo/react-hooks';
import { GET_MY_TODOS } from './TodoPrivateList';
import { 
  GetMyTodosQuery,
  RemoveTodoMutationFn,
  Todos
} from '../../generated/graphql';

interface TodoItemType {
  index: number,
  todo: Partial<Todos>
};

const TOGGLE_TODO = gql`
  mutation toggleTodo ($id: Int!, $isCompleted: Boolean!) {
    update_todos(where: {id: {_eq: $id}}, _set: {is_completed: $isCompleted}) {
      affected_rows
    }
  }
`;

const REMOVE_TODO = gql`
  mutation removeTodo ($id: Int!) {
    delete_todos(where: {id: {_eq: $id}}) {
      affected_rows
    }
  }
`;

const TodoItem = ({index, todo}: TodoItemType) => {

  const [todoUpdate] = useMutation(
    TOGGLE_TODO, 
    {
      update(cache, { data }) {
        const existingTodos = cache.readQuery<GetMyTodosQuery>({ query: GET_MY_TODOS });
        const newTodos = existingTodos!.todos.map(t => {
          if (t.id === todo.id) {
            return({...t, is_completed: !t.is_completed});
          } else {
            return t;
          }
        });
        cache.writeQuery<GetMyTodosQuery>({
          query: GET_MY_TODOS,
          data: {todos: newTodos}
        });
      }
    }
  );

  const [todoRemove] = useMutation<RemoveTodoMutationFn>(
    REMOVE_TODO, 
    {
      update(cache, { data }) {
        const existingTodos = cache.readQuery<GetMyTodosQuery>({ query: GET_MY_TODOS });
        const newTodos = existingTodos!.todos.filter(t => (t.id !== todo.id));
        cache.writeQuery<GetMyTodosQuery>({
          query: GET_MY_TODOS,
          data: {todos: newTodos}
        });
      }
    }
  );

  const removeTodo = (e: React.MouseEvent) => {
    e.preventDefault();
    e.stopPropagation();
    todoRemove({
      variables: { id: todo.id },
    });
  };

  const toggleTodo = () => {
    todoUpdate({
      variables: { id: todo.id, isCompleted: !todo.is_completed },
      optimisticResponse: {
        __typename: "Mutation",
        update_todos: {
          __typename: "todos_mutation_response",
          id: todo.id,
          title: todo.title,
          is_completed: todo.is_completed,
          affected_rows: 1
        }
      }
    });
  };

  return (
    <li key={index}>
      <div className="view">
        <div className="round">
          <input
            checked={todo.is_completed}
            type="checkbox"
            id={todo.id!.toString()}
            onChange={() => toggleTodo()}
          />
          <label htmlFor={todo.id!.toString()}/>
        </div>
      </div>

      <div className={"labelContent" + (todo.is_completed ? " completed" : '')}>
        <div>
          {todo.title}
        </div>
      </div>

      <button className="closeBtn" onClick={(e) => removeTodo(e)}>
        x
      </button>
    </li>
  );
};

export default TodoItem;
