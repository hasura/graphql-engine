import React from "react";
import PropTypes from "prop-types";
import "../../styles/App.css";

class TodoInput extends React.Component {
  constructor() {
    super();
    this.state = {
      textboxValue: ""
    };
    this.handleTextboxValueChange = this.handleTextboxValueChange.bind(this);
  }

  handleTextboxValueChange(e) {
    this.setState({
      ...this.state,
      textboxValue: e.target.value
    });
  }

  render() {
    return (
      <div className="formInput">
        <input
          className="input"
          data-test={
            this.props.type === "private"
              ? "input-private"
              : "input-public"
          }
          placeholder="What needs to be done?"
          value={this.state.textboxValue}
          onChange={this.handleTextboxValueChange}
        />
        <i className="downArrow fa fa-angle-down" />
      </div>
    );
  }
}

TodoInput.propTypes = {
  userId: PropTypes.string,
  type: PropTypes.string
};

export default TodoInput;
