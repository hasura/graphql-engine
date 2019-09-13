import React, { Fragment, useState, useRef, useEffect } from "react";
import { useSubscription, useApolloClient } from "@apollo/react-hooks";
import gql from "graphql-tag";
import TaskItem from "./TaskItem";

import {
  GetOldPublicTodosQuery,
  GetOldPublicTodosQueryVariables,
  GetNewPublicTodosQuery,
  GetNewPublicTodosQueryVariables,
  NotifyNewPublicTodosSubscription,
  Todos
} from '../../generated/graphql';

type publicListProps = {
  latestTodo?: Partial<Todos> | null
}

const TodoPublicList = (props: publicListProps) => {
  const [olderTodosAvailable, setOlderTodosAvailable] = useState(props.latestTodo ? true : false)
  const [newTodosCount, setNewTodosCount] = useState(0)
  const [error, setError] = useState(false);
  const [todos, setTodos] = useState<GetOldPublicTodosQuery["todos"]>([]);

  let oldestTodoId = useRef(props.latestTodo && props.latestTodo.id ? props.latestTodo.id + 1 : 0);
  let newestTodoId = useRef(props.latestTodo && props.latestTodo.id ? props.latestTodo.id : 0);
  if(todos && todos.length) {
    oldestTodoId.current = todos[todos.length - 1].id
    newestTodoId.current = todos[0].id;
  }

  const client = useApolloClient();

  const loadOlder = async () => {
    const GET_OLD_PUBLIC_TODOS = gql`
      query getOldPublicTodos($oldestTodoId: Int!) {
        todos(
          where: { is_public: { _eq: true }, id: { _lt: $oldestTodoId } }
          limit: 7
          order_by: { created_at: desc }
        ) {
          id
          title
          created_at
          user {
            name
          }
        }
      }
    `;

    const { data, networkStatus } = await client.query<GetOldPublicTodosQuery, GetOldPublicTodosQueryVariables>({
      query: GET_OLD_PUBLIC_TODOS,
      variables: { oldestTodoId: oldestTodoId.current }
    });
    if (data.todos && data.todos.length) {
      setTodos(prevTodos => {
        if(prevTodos) {
          return [...prevTodos, ...data.todos];
        } else {
          return data.todos;
        }
      });
      oldestTodoId.current = data.todos[data.todos.length - 1].id;
    } else {
      setOlderTodosAvailable(false);
    }
    if (networkStatus === 8) {
      console.error(data);
      setError(true);
    }
  };

  const loadNew = async () => {
    const GET_NEW_PUBLIC_TODOS = gql`
      query getNewPublicTodos($latestVisibleId: Int) {
        todos(
          where: { is_public: { _eq: true }, id: { _gt: $latestVisibleId } }
          order_by: { created_at: desc }
        ) {
          id
          title
          created_at
          user {
            name
          }
        }
      }
    `;

    const { data, networkStatus } = await client.query<GetNewPublicTodosQuery, GetNewPublicTodosQueryVariables>({
      query: GET_NEW_PUBLIC_TODOS,
      variables: {
        latestVisibleId: newestTodoId.current
      }
    });

    if (data && data.todos) {
      setTodos(prevState => {
        if(prevState) {
          return [...data.todos, ...prevState]
        } else {
          return data.todos;
        }
      });
      setNewTodosCount(0);
      newestTodoId.current = data.todos[0].id;
    }
    if (networkStatus === 8) {
      console.error(data);
      setError(true);
    }
  };

  useEffect(() => {
    loadOlder();
    // eslint-disable-next-line
  }, []);

  useEffect(
    () => {
      if (props.latestTodo && props.latestTodo.id! > newestTodoId.current) {
        setNewTodosCount(n => n + 1);
        newestTodoId.current = props.latestTodo.id!;
      }
    },
    [props.latestTodo]
  );

  if(error) {
    return (<div>Error...</div>);
  }

  return (
    <Fragment>
      <div className="todoListWrapper">
        {newTodosCount !== 0 && (
          <div className={"loadMoreSection"} onClick={() => loadNew()}>
            New tasks have arrived! ({newTodosCount.toString()})
          </div>
        )}

        <ul>
          {todos &&
            todos.map((todo, index) => {
              return <TaskItem key={index} index={index} todo={todo} />;
            })}
        </ul>

        <div className={"loadMoreSection"} onClick={() => loadOlder()}>
          {olderTodosAvailable
            ? "Load older tasks"
            : "No more public tasks!"}
        </div>
      </div>
    </Fragment>
  );
};

const TodoPublicListSubscription = () => {
  // Run a subscription to get the latest public todo
  const NOTIFY_NEW_PUBLIC_TODOS = gql`
    subscription notifyNewPublicTodos {
      todos(
        where: { is_public: { _eq: true } }
        limit: 1
        order_by: { created_at: desc }
      ) {
        id
        created_at
      }
    }
  `;

  const { loading, error, data } = useSubscription<NotifyNewPublicTodosSubscription>(NOTIFY_NEW_PUBLIC_TODOS);
  if (loading) {
    return <span>Loading...</span>;
  }
  if (error || !data) {
    return <span>Error</span>;
  }
  return (
    <TodoPublicList latestTodo={data.todos.length ? data.todos[0] : null} />
  );
};

export default TodoPublicListSubscription;