import React from 'react';
import LoginContainer from './LoginContainer';
import AdminSecretLoginForm from './AdminSecretLoginForm';

const Login: React.FC = () => {
  return (
    <LoginContainer>
      <AdminSecretLoginForm />
    </LoginContainer>
  );
};

export default Login;
