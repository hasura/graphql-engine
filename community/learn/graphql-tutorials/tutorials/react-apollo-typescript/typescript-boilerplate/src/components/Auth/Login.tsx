import * as React from 'react';
import { Button } from "react-bootstrap";

const Login = ({loginHandler} : {loginHandler: VoidFunction}) => (
<div className="overlay">
  <div className="overlay-content">
    <div className="overlay-heading">
      Welcome to the GraphQL tutorial app
    </div>
    <div className="overlay-message">
      Please login to continue
    </div>
    <div className="overlay-action">
      <Button
        id="qsLoginBtn"
        className="btn-margin loginBtn"
        onClick={() => {loginHandler()}}
       >
        Log In
      </Button>
    </div>
  </div>
</div>
);

export default Login;
