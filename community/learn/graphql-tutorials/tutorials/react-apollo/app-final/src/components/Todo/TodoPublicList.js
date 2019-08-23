import React, { Component, Fragment } from 'react';
import {Subscription, withApollo} from 'react-apollo';
import gql from 'graphql-tag';
import TaskItem from './TaskItem';

class _TodoPublicList extends Component {
  constructor(props) {
    super(props);

    this.state = {
      olderTodosAvailable: props.latestTodo ? true : false,
      newTodosCount: 0,
      error: false,
      todos: [],
    };

    this.loadNew = this.loadNew.bind(this);
    this.loadOlder = this.loadOlder.bind(this);

    this.client = props.client;
    this.oldestTodoId = props.latestTodo ? (props.latestTodo.id + 1) : 0;
    this.newestTodoId = props.latestTodo ? props.latestTodo.id : 0;
  }

  loadNew() {
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

    this.client.query({
      query: GET_NEW_PUBLIC_TODOS,
      variables: {latestVisibleId: this.state.todos.length ? this.state.todos[0].id : null}
    })
      .then(({data}) => {
        this.newestTodoId = data.todos[0].id;
        this.setState({todos: [...data.todos, ...this.state.todos], newTodosCount: 0});
      })
      .catch(error => {
        console.error(error);
        this.setState({error: true});
      });
  }

  loadOlder() {
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

    this.client.query({
      query: GET_OLD_PUBLIC_TODOS,
      variables: {oldestTodoId: (this.oldestTodoId)}
    })
      .then(({data}) => {
        if (data.todos.length) {
          this.oldestTodoId = data.todos[data.todos.length - 1].id;
          this.setState({todos: [...this.state.todos, ...data.todos]});
        } else {
          this.setState({olderTodosAvailable: false});
        }
      })
      .catch(error => {
        console.error(error);
        this.setState({error: true});
      });
  }

  componentDidUpdate(prevProps) {
    // Do we have a new todo available?
    if (this.props.latestTodo && this.props.latestTodo.id > this.newestTodoId) {
      this.newestTodoId = this.props.latestTodo.id;
      this.setState({newTodosCount: this.state.newTodosCount + 1});
    }
  }

  componentDidMount() {
    this.loadOlder();
  }

  render() {

    let todos = this.state.todos;

    const todoList = (
      <ul>
        {
          todos.map((todo, index) => {
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
    );

    let newTodosNotification = '';
    if (this.state.newTodosCount) {
      newTodosNotification = (
        <div className={"loadMoreSection"} onClick={this.loadNew}>
          New tasks have arrived! ({this.state.newTodosCount.toString()})
        </div>
      );
    }

    const olderTodosMsg = (
      <div className={"loadMoreSection"} onClick={this.loadOlder}>
        { this.state.olderTodosAvailable ? 'Load older tasks' : 'No more public tasks!'}
      </div>
    );

    return (
      <Fragment>
        <div className="todoListWrapper">
          { newTodosNotification }

          { todoList }

          { olderTodosMsg }
        </div>
      </Fragment>
    );
  }
}

const TodoPublicList = withApollo(_TodoPublicList);

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
  return (
    <Subscription subscription={NOTIFY_NEW_PUBLIC_TODOS}>
      {({loading, error, data}) => {
        if (loading) {
          return (<span>Loading...</span>);
        }
        if (error) {
          return (<span>Error</span>);
        }
        return (<TodoPublicList latestTodo={data.todos.length ? data.todos[0] : null} />);
      }}
    </Subscription>
  );
};
export default TodoPublicListSubscription;
