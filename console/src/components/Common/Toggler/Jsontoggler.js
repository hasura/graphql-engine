import React, { Component } from 'react';
import AceEditor from 'react-ace';

const styles = require('../Common.scss');

class Jsontoggler extends Component {
  constructor(props) {
    super(props);

    this.state = {
      input_json: 0,
      input_value: this.props.standardProps.defaultValue,
    };

    this.handleClick = this.handleClick.bind(this);
  }

  handleClick(e) {
    e.preventDefault();
    const stateValue = this.state.input_json;
    if (stateValue === 0) {
      this.setState({ input_json: 1 });
    } else {
      this.setState({ input_json: 0 });
    }
  }

  render() {
    return (
      <span>
        <label>
          {this.state.input_json === 1 ? (
            <AceEditor
              {...this.props.standardProps}
              mode="json"
              theme="github"
              name="jsontoggler"
              minLines={10}
              maxLines={100}
              width="100%"
              value={this.state.input_value}
              onChange={e => {
                this.setState({ input_value: e });
              }}
              showPrintMargin={false}
              showGutter={false}
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
            (this.state.input_json === 1
              ? 'fa fa-compress '
              : 'fa fa-expand ') + styles.jsonButtonAlign
          }
          onClick={this.handleClick}
        />
      </span>
    );
  }
}

export default Jsontoggler;
