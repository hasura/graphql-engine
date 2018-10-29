import React from 'react';
import PropTypes from 'prop-types';

import { inputChecker } from './utils';

export default class InputChecker extends React.Component {
  constructor() {
    super();
    this.state = {
      isError: false,
      errorMessage: '',
    };
  }
  onBlur(e) {
    const val = e.target.value;
    const indexId =
      e.target && parseInt(e.target.getAttribute('data-index-id'), 10);
    if (!val) {
      this.setState({
        ...this.state,
        isError: false,
        errorMessage: '',
      });
      return;
    }
    inputChecker(this.props.type, val)
      .then(r => {
        this.setState({
          ...this.state,
          isError: false,
          errorMessage: '',
        });
        this.props.onBlur(undefined, indexId, r);
      })
      .catch(r => {
        this.setState({
          ...this.state,
          isError: true,
          errorMessage: r.message,
        });
      });
  }
  render() {
    const { value, onChange, placeholder, indexId, disabled } = this.props;

    const style = {
      border: '1px solid red',
      cursor: 'pointer',
    };
    return (
      <input
        className={'input-sm form-control'}
        style={this.state.isError ? style : {}}
        placeholder={placeholder || 'new input'}
        value={value}
        onChange={onChange}
        onBlur={this.onBlur.bind(this)}
        data-index-id={indexId || 0}
        disabled={disabled}
        title={this.state.errorMessage || ''}
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
  indexId: PropTypes.number,
  disabled: PropTypes.bool,
};
