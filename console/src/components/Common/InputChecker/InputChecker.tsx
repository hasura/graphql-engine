import React, { Component } from 'react';

import { inputChecker } from './utils';

type Props = {
  type: string;
  value: string;
  placeholder: string;
  disabled: boolean;
  title: string;
  onBlur?: (e: React.FocusEvent<HTMLInputElement>) => void;
  onChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
  'data-test': string;
};

type State = {
  isError: boolean;
  errorMessage: string;
};

class InputChecker extends Component<Props, State> {
  state = {
    isError: false,
    errorMessage: '',
  };

  onBlur = (e: React.FocusEvent<HTMLInputElement>) => {
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
  };
  render() {
    const { value, onChange, placeholder, disabled, title } = this.props;

    const style = {
      border: '1px solid red',
      cursor: 'pointer',
    };
    return (
      <input
        {...this.props}
        className="input-sm form-control"
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

export default InputChecker;
