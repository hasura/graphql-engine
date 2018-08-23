import React from 'react';

const LoginComponent = (props) => {
  return (
    <div>    
      <h1 className="loginHeading"> Welcome to the sample chat app made with Hasura GraphQL Engine </h1>
      <div className="login">
        <div>
          <form
            submit={props.login}
          >
            <input
              type="text"
              id="username"
              className="loginTextbox"
              placeholder="Username"
              value={props.username}
              onChange={(e) => props.setUsername(e.target.value)}
            />
            <button
              className="loginButton"
              onClick={props.login}
            >
              Enter
            </button> 
          </form>
        </div>
      </div>
    </div>
  );
};

export default LoginComponent;