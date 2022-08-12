import React, { Component } from 'react';
import PropTypes from 'prop-types';

import { inputChecker } from './utils';

class InputChecker extends Component {
  constructor() {
    super();
    this.state = {
      isError: false,
      errorMessage: '',
    };

    this.onBlur = this.onBlur.bind(this);
  }
  onBlur(e) {
    const val = e.target.value;
    if (!val) {
      this.setState({
        isError: false,
        errorMessage: '',
      });
      return;
    }
    inputChecker(this.props.type, val)
      .then(() => {
        this.setState({
          isError: false,
          errorMessage: '',
        });
      })
      .catch(r => {
        this.setState({
          isError: true,
          errorMessage: r.message,
        });
      });
  }
  render() {
    const { value, onChange, placeholder, disabled, title } = this.props;

    const style = {
      border: '1px solid red',
      cursor: 'pointer',
    };
    return (
      <input
        {...this.props}
        className={'input-sm form-control'}
        style={this.state.isError ? style : {}}
        placeholder={placeholder || 'new input'}
        value={value}
        onChange={onChange}
        onBlur={this.onBlur}
        disabled={disabled}
        title={this.state.errorMessage || title}
        data-test={this.props['data-test']}
      />
    );
  }
}

InputChecker.propTypes = {
  type: PropTypes.string.isRequired,
  value: PropTypes.string.isRequired,
  onChange: PropTypes.func.isRequired,
  onBlur: PropTypes.func.isRequired,
  placeholder: PropTypes.string,
  disabled: PropTypes.bool,
};

export default InputChecker;
