import React, { Fragment, useState } from "react";
import gql from "graphql-tag";
import { useQuery, useMutation } from "@apollo/react-hooks";

import TodoItem from "./TodoItem";
import TodoFilters from "./TodoFilters";

import { 
  GetMyTodosQuery,
  ClearCompletedMutation
} from '../../generated/graphql';

const GET_MY_TODOS = gql`
  query getMyTodos {
    todos(where: { is_public: { _eq: false} }, order_by: { created_at: desc }) {
      id
      title
      created_at
      is_completed
    }
  }
`;

 const CLEAR_COMPLETED = gql`
  mutation clearCompleted {
    delete_todos(where: {is_completed: {_eq: true}, is_public: {_eq: false}}) {
      affected_rows
    }
  }
`;

const TodoPrivateList = () => {

  const [filter, setFilter] = useState<string>("all");
  const { loading, error, data } = useQuery<GetMyTodosQuery>(GET_MY_TODOS);
  const [clearTodos] = useMutation<ClearCompletedMutation>(
      CLEAR_COMPLETED, 
      {
        update(cache, { data }) {
          const existingTodos: any = cache.readQuery<GetMyTodosQuery>({ query: GET_MY_TODOS });
          const newTodos = existingTodos.todos.filter((t:any) => (!t.is_completed));
          cache.writeQuery<GetMyTodosQuery>({query:GET_MY_TODOS, data: {todos: newTodos}});
        }
      });

  const filterResults = (filter: string): void => {
    setFilter(filter);
  };

  const clearCompleted = (): void => {
    clearTodos();
  }

  if(loading) {
    return (<div>Loading...</div>);
  }
  if(error || !data) {
    return (<div>Error...</div>);
  }

  let filteredTodos = data.todos;
  console.log(filter);
  if (filter === "active") {
    filteredTodos = data.todos.filter(todo => todo.is_completed !== true);
  } else if (filter === "completed") {
    filteredTodos = data.todos.filter(todo => todo.is_completed === true);
  }

  const todoList = [] as any;
  filteredTodos.forEach((todo: any, index: number) => {
    todoList.push(
      <TodoItem
        key={'item'+index}
        index={index}
        todo={todo}
      />
    );
  });

  return (
    <Fragment>
      <div className="todoListWrapper">
        <ul>
          { todoList }
        </ul>
      </div>

      <TodoFilters
        todos={filteredTodos}
        currentFilter={filter}
        filterResultsFn={filterResults}
        clearCompletedFn={clearCompleted}
      />
    </Fragment>
  );
}

export default TodoPrivateList;
export { GET_MY_TODOS };
