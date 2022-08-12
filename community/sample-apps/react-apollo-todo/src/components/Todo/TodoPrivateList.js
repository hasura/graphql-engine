import React, { Component, Fragment } from "react";
import PropTypes from "prop-types";
import { Query } from "react-apollo";
import { GRAPHQL_URL } from "../../utils/constants";
import TodoItem from "./TodoItem";
import TodoFilters from "./TodoFilters";
import { QUERY_PRIVATE_TODO } from "./TodoQueries";

class TodoPrivateList extends Component {
  constructor() {
    super();
    this.state = { filter: "all", clearInProgress: false };
  }
  filterResults(type) {
    this.setState({ filter: type });
  }
  clearCompleted(type) {
    // mutation to delete all is_completed with is_public clause
    const isOk = window.confirm("Are you sure?");
    if (isOk) {
      this.setState({ clearInProgress: true });
      const isPublic = type === "public" ? true : false;
      this.props.client
        .query({
          query: `
            mutation ($isPublic: Boolean!) {
              delete_todos (
                where: { is_completed: {_eq: true}, is_public: {_eq: $isPublic}}
              ) {
                affected_rows
              }
            }
          `,
          endpoint: GRAPHQL_URL,
          variables: {
            isPublic: isPublic
          }
        })
        .then(() => {
          // handle response
          this.setState({ clearInProgress: false });
        })
        .catch(error => {
          this.setState({ clearInProgress: false });
          console.error(error);
        });
    }
  }
  render() {
    const { userId, type } = this.props;
    return (
      <Query query={QUERY_PRIVATE_TODO} variables={{ userId: userId }}>
        {({ loading, error, data, refetch }) => {
          if (loading) {
            return <div>Loading. Please wait...</div>;
          }
          if (error) {
            return <div>{""}</div>;
          }
          refetch();
          // apply filters for displaying todos
          let finalData = data.todos;
          if (this.state.filter === "active") {
            finalData = data.todos.filter(todo => todo.is_completed !== true);
          } else if (this.state.filter === "completed") {
            finalData = data.todos.filter(todo => todo.is_completed === true);
          }
          return (
            <Fragment>
              <div className="todoListwrapper">
                <ul>
                  {finalData.map((todo, index) => {
                    return (
                      <TodoItem
                        key={index}
                        index={index}
                        todo={todo}
                        type={type}
                        userId={userId}
                      />
                    );
                  })}
                </ul>
              </div>
              <TodoFilters
                todos={data.todos}
                userId={userId}
                type={type}
                currentFilter={this.state.filter}
                filterResults={this.filterResults.bind(this)}
                clearCompleted={this.clearCompleted.bind(this)}
                clearInProgress={this.state.clearInProgress}
              />
            </Fragment>
          );
        }}
      </Query>
    );
  }
}

TodoPrivateList.propTypes = {
  userId: PropTypes.string.isRequired,
  type: PropTypes.string.isRequired
};

export default TodoPrivateList;
