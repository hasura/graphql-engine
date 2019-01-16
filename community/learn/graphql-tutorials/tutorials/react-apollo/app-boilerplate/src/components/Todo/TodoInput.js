import React, { Component } from "react";
import PropTypes from "prop-types";

class TodoInput extends Component {
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
    const { type } = this.props;

    return (
      <div className="formInput">
        <input
          className="input"
          data-test={type === "private" ? "input-private" : "input-public"}
          placeholder="What needs to be done?"
          value={this.state.textboxValue}
          onChange={this.handleTextboxValueChange}
        />
        <i className="inputMarker fa fa-angle-right" />
      </div>
    );
  }
}

TodoInput.propTypes = {
  type: PropTypes.string
};

export default TodoInput;
