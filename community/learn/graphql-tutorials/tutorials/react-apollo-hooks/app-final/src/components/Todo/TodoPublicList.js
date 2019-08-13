import React, { Component, Fragment, useState, useEffect } from 'react';
import { useQuery, useSubscription, useApolloClient } from '@apollo/react-hooks';
import gql from 'graphql-tag';
import TaskItem from './TaskItem';

const TodoPublicList = (props) => {
  const [state, setState] = useState({
    olderTodosAvailable: props.latestTodo ? true : false,
    newTodosCount: 0,
    error: false,
    todos: [],
    oldestTodoId: props.latestTodo ? (props.latestTodo.id + 1) : 0,
    newestTodoId: props.latestTodo ? props.latestTodo.id : 0, 
  });
  
  const client = useApolloClient();

  useEffect(() => {
    loadOlder();
  }, []);

  useEffect(() => {
    if (props.latestTodo && props.latestTodo.id > state.newestTodoId) {
      setState({...state, newestTodoId: props.latestTodo.id, newTodosCount: state.newTodosCount + 1});
    }
  }, [props.latestTodo]);

  const loadOlder = async () => {
    const GET_OLD_PUBLIC_TODOS = gql`
      query getOldPublicTodos ($oldestTodoId: Int!) {
        todos (where: { is_public: { _eq: true}, id: {_lt: $oldestTodoId}}, limit: 7, order_by: { created_at: desc }) {
          id
          title
          created_at
          user {
            name
          }
        }
      }`;

    const { loading, error, data} = await client.query({query: GET_OLD_PUBLIC_TODOS, 
      variables: {oldestTodoId: state.oldestTodoId}
    });

          if (data.todos.length) {
            setState({...state , oldestTodoId: data.todos[data.todos.length - 1].id, todos: [...state.todos, ...data.todos]});
          } else {
            setState({...state, olderTodosAvailable: false});
          }
        if (error) {
          console.error(error);
          setState({...state, error: true});
        }        
  }

  const loadNew = async () => {
    const GET_NEW_PUBLIC_TODOS = gql`
      query getNewPublicTodos ($latestVisibleId: Int) {
        todos(where: { is_public: { _eq: true}, id: {_gt: $latestVisibleId}}, order_by: { created_at: desc }) {
          id
          title
          created_at
          user {
            name
          }
        }
      }
    `;

const { loading, error, data } = await client.query({query: GET_NEW_PUBLIC_TODOS,
  variables: {latestVisibleId: state.todos.length ? state.todos[0].id : null} 
});   

      if(data) {  
        setState({...state, newestTodoId:data.todos[0].id, todos: [...data.todos, ...state.todos], newTodosCount: 0});
      }
      if(error){
        console.error(error);
        setState({...state, error: true});
      }
  }

    return (
      <Fragment>
        <div className="todoListWrapper">
        {state.newTodosCount !=0 && <div className={"loadMoreSection"} onClick={loadNew}>
          New tasks have arrived! ({state.newTodosCount.toString()})
        </div>}

          <ul>
        {
          state.todos && state.todos.map((todo, index) => {
            return (
              <TaskItem
                key={index}
                index={index}
                todo={todo}
              />
            );
          })
        }
      </ul> 

      <div className={"loadMoreSection"} onClick={loadOlder}>
        { state.olderTodosAvailable ? 'Load older tasks' : 'No more public tasks!'}
      </div> 
        </div>
      </Fragment>
    );
} 

// Run a subscription to get the latest public todo
const NOTIFY_NEW_PUBLIC_TODOS = gql`
  subscription notifyNewPublicTodos {
    todos (where: { is_public: { _eq: true}}, limit: 1, order_by: {created_at: desc }) {
      id
      created_at
    }
  }
`;

const TodoPublicListSubscription = () => {
  const { loading, error, data } = useSubscription(NOTIFY_NEW_PUBLIC_TODOS);
        if (loading) {
          return (<span>Loading...</span>);
        }
        if (error) {
          return (<span>Error</span>);
        }
        return (<TodoPublicList latestTodo={data.todos.length ? data.todos[0] : null} />);
};
export default TodoPublicListSubscription;
