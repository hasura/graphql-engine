import React from 'react';
import LoginForm from '../Util/LoginForm';

class Login extends React.Component {

  render() {
    return (
      <LoginForm type="login" submit={this.props.submit} {...this.props}/>
    );
  }
}

export default Login;
