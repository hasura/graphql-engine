import React, { Component, Fragment } from "react";
import PropTypes from "prop-types";
import TodoItem from "./TodoItem";
import TodoFilters from "./TodoFilters";

class TodoPrivateList extends Component {
  constructor() {
    super();
    this.state = { filter: "all", clearInProgress: false };
  }
  filterResults(type) {
    this.setState({ ...this.state, filter: type });
  }
  render() {
    const { userId, type } = this.props;
    const data = [
      {
        id: "1",
        text: "This is private todo 1",
        is_completed: true,
        is_public: false
      },
      {
        id: "2",
        text: "This is private todo 2",
        is_completed: false,
        is_public: false
      }
    ];
    // apply filters for displaying todos
    let finalData = data;
    if (this.state.filter === "active") {
      finalData = data.filter(todo => todo.is_completed !== true);
    } else if (this.state.filter === "completed") {
      finalData = data.filter(todo => todo.is_completed === true);
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
          todos={finalData}
          userId={userId}
          type={type}
          currentFilter={this.state.filter}
          filterResults={this.filterResults.bind(this)}
          clearInProgress={this.state.clearInProgress}
        />
      </Fragment>
    );
  }
}

TodoPrivateList.propTypes = {
  userId: PropTypes.string.isRequired,
  type: PropTypes.string.isRequired
};

export default TodoPrivateList;
