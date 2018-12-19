import React, { Component } from 'react';

const styles = require('../Common.scss');

class Toggler extends Component {
  constructor(props) {
    super(props);

    this.state = {
      input_textarea: 0,
      input_value: this.props.standardProps.defaultValue,
    };

    this.handleClick = this.handleClick.bind(this);
  }

  handleClick(e) {
    e.preventDefault();
    const stateValue = this.state.input_textarea;
    if (stateValue === 0) {
      this.setState({ input_textarea: 1 });
    } else {
      this.setState({ input_textarea: 0 });
    }
  }

  render() {
    return (
      <span>
        <label>
          {this.state.input_textarea === 1 ? (
            <textarea
              {...this.props.standardProps}
              placeholder={this.props.placeholderProp}
              value={this.state.input_value}
              onChange={e => {
                this.setState({ input_value: e.target.value });
              }}
              rows="5"
              cols="5"
            />
          ) : (
            <input
              {...this.props.standardProps}
              placeholder={this.props.placeholderProp}
              value={this.state.input_value}
              onChange={e => {
                this.setState({ input_value: e.target.value });
              }}
            />
          )}
        </label>
        <i
          className={
            this.state.input_textarea === 1
              ? 'fa fa-compress' + ' ' + styles.textareaButtonAlign
              : 'fa fa-expand' + ' ' + styles.textareaButtonAlign
          }
          onClick={this.handleClick}
        />
      </span>
    );
  }
}

export default Toggler;
