import React, { Fragment, useState } from "react";
import { useQuery, useMutation } from "@apollo/react-hooks";
import gql from "graphql-tag";

import TodoItem from "./TodoItem";
import TodoFilters from "./TodoFilters";

const GET_MY_TODOS = gql`
  query getMyTodos {
    todos(
      where: { is_public: { _eq: false } }
      order_by: { created_at: desc }
    ) {
      id
      title
      created_at
      is_completed
    }
  }
`;

// Remove all the todos that are completed
const CLEAR_COMPLETED = gql`
  mutation clearCompleted {
    delete_todos(
      where: { is_completed: { _eq: true }, is_public: { _eq: false } }
    ) {
      affected_rows
    }
  }
`;

const TodoPrivateList = props => {
  const [state, setState] = useState({
    filter: "all",
    clearInProgress: false
  });

  const filterResults = filter => {
    setState({
      ...state,
      filter: filter
    });
  };

  const [clearCompletedTodos] = useMutation(CLEAR_COMPLETED);

  const clearCompleted = () => {
    clearCompletedTodos({
      optimisticResponse: {},
      update: (cache, { data }) => {
        const existingTodos = cache.readQuery({ query: GET_MY_TODOS });
        const newTodos = existingTodos.todos.filter(t => !t.is_completed);
        cache.writeQuery({ query: GET_MY_TODOS, data: { todos: newTodos } });
      }
    });
  };

  const { todos } = props;

  let filteredTodos = todos;
  if (state.filter === "active") {
    filteredTodos = todos.filter(todo => todo.is_completed !== true);
  } else if (state.filter === "completed") {
    filteredTodos = todos.filter(todo => todo.is_completed === true);
  }

  const todoList = [];
  filteredTodos.forEach((todo, index) => {
    todoList.push(<TodoItem key={index} index={index} todo={todo} />);
  });

  return (
    <Fragment>
      <div className="todoListWrapper">
        <ul>{todoList}</ul>
      </div>

      <TodoFilters
        todos={filteredTodos}
        currentFilter={state.filter}
        filterResultsFn={filterResults}
        clearCompletedFn={clearCompleted}
        clearInProgress={state.clearInProgress}
      />
    </Fragment>
  );
};

const TodoPrivateListQuery = () => {
  const { loading, error, data } = useQuery(GET_MY_TODOS);

  if (loading) {
    return <div>Loading...</div>;
  }
  if (error) {
    console.error(error);
    return <div>Error!</div>;
  }
  return <TodoPrivateList todos={data.todos} />;
};
export default TodoPrivateListQuery;
export { GET_MY_TODOS };
