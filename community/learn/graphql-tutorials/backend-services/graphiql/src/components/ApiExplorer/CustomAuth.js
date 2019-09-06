import React from 'react';
import styles from './ApiExplorer.scss';
import { signup, login } from './customAuthActions';
import { emailRegex } from './utils';
import jwtDecoder from 'jwt-decode';

class CustomAuth extends React.Component {

  state = {
    tabIndex: 0,
    email: "",
    password: "",
    loading: false
  }

  handleEmailChange = (e) => {
    this.setState({
      email: e.target.value
    })
  }

  handlePasswordChange = (e) => {
    this.setState({
      password: e.target.value
    });
  }

  performSignup = (e, p) => {
    const successCallback = () => {      this.setState({
        loading: false
      })

      this.setState({
        tabIndex: 0,
        loading: false
      });
      alert('Successfully signed up! Please login.')
    }
    const errorCallback = (e) => {
      this.setState({
        loading: false
      });
      alert(
        `
        ${e.title}
        ${e.message}
        `
      )
    }
    signup(this.state.email, this.state.password, successCallback, errorCallback);
  }

  performLogin = () => {
    const successCallback = (response) => {
      this.setState({
        tabIndex: 0,
        email: '',
        password: '',
      });
      const decodedToken = jwtDecoder(response.token);
      window.localStorage.setItem('@learn.hasura.io:graphiql-react-native-token', response.token);
      window.localStorage.setItem('@learn.hasura.io:graphiql-react-native-exp', decodedToken.exp);
      window.location.replace(window.location.href);
    }
    const errorCallback = (e) => {
      this.setState({
        loading: false
      })
      alert(
        `
        ${e.title}
        ${e.message}
        `
      )
    }
    login(this.state.email, this.state.password, successCallback, errorCallback);
  }

  submit = () => {
    this.setState({ loading: true });
    if (this.state.tabIndex === 0) {
      this.performLogin(this.state.email, this.state.password)
    } else {
      this.performSignup(this.state.email, this.state.password)
    }
  }

  toggleTab = (i) => {
    this.setState({
      tabIndex: i
    });
  }

  render() {
    const { email, password, tabIndex, loading } = this.state;
    const { handleEmailChange, handlePasswordChange } = this;
    const toggleLogin = () => this.toggleTab(0);
    const toggleSignup = () => this.toggleTab(1);
    let buttonText = tabIndex === 0 ? 'Log in' : 'Sign up';
    if (loading) {
      buttonText = "Please wait...";
    }
    let loginTabStyle, signupTabStyle;
    if (tabIndex === 0) {
      loginTabStyle = styles.loginTabActive;
      signupTabStyle = styles.loginTabInactive;
    } else {
      loginTabStyle = styles.loginTabInactive;
      signupTabStyle = styles.loginTabActive;
    }
    return (
      <div className={styles.loginTabs}>
        <div className={styles.loginTabsHeader}>
          <div className={loginTabStyle} onClick={toggleLogin}>
            Login
          </div>
          <div className={signupTabStyle} onClick={toggleSignup}>
            Signup
          </div>
        </div>
        <div className={styles.loginFormWrapper}>
          <Form
            email={email}
            password={password}
            handleEmailChange={handleEmailChange}
            handlePasswordChange={handlePasswordChange}
            buttonText={buttonText}
            loading={loading}
            submit={this.submit}
          />
        </div>
      </div>
    )
  }
}

const Form = ({
  email,
  handleEmailChange,
  password,
  handlePasswordChange,
  submit,
  loading,
  buttonText
}) => {

  const validateAndSubmit = (e) => {
    e.preventDefault();
    if (!emailRegex.test(email.toLowerCase())) {
      alert('Invalid email', 'Please enter a valid email address');
      return;
    }
    if (!email || !password) {
      alert('Email or password cannot be empty');
      return;
    }
    submit();
  }

  return (
    <form
      onSubmit={validateAndSubmit}
    >
      <div className={styles.loginFormElement}>
        <input
          value={email}
          type="text"
          onChange={handleEmailChange}
          placeholder="Email"
          className="form-control"
        />
      </div>
      <div className={styles.loginFormElement}>
        <input
          value={password}
          type="password"
          onChange={handlePasswordChange}
          placeholder="Password"
          className="form-control"
        />
      </div>
      <div className={styles.loginFormElement}>
        <button
          type="submit"
          className="btn btn-primary"
          disabled={loading}
        >
          {buttonText}
        </button>
      </div>      
    </form>
  )
}

export default CustomAuth;