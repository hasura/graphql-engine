import React from 'react';
import '../App.css';
const LoginComponent = (props) => {
  return (
    <div className="loginWrapper">
      <h2 className="loginHeading"> Welcome to sample chat app made with Hasura GraphQL Engine </h2>
      <div className="login">
        <form>
          <input
            type="text"
            id="username"
            className="loginTextbox"
            placeholder="Username"
            autoFocus={true}
            value={props.username}
            onChange={(e) => props.setUsername(e.target.value)}
          />
          <button
            className="loginButton"
            type="submit"
            onClick={props.login}
          >
            Enter
          </button>
        </form>
      </div>
    </div>
  );
};

export default LoginComponent;
