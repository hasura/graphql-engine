import React from 'react';

const LoginComponent = (props) => {
  return (
    <div className="login">
      <div>
        <input
          type="text"
          id="username"
          placeholder="Username"
          value={props.username}
          onChange={(e) => props.setUsername(e.target.value)}
        />
        <button onClick={props.login}>
          Enter
        </button> 
      </div>
    </div>
  );
};

export default LoginComponent;