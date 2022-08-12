import React from 'react';
import NavBar from './components/Navbar';
import { LoadingIndicator } from './components/Utils';
import './App.css';
import Router from './Router';
import { withApollo } from 'react-apollo';
import { useUsername } from './utils';

function App({client}) {

  // set username in database and in localstorage
  const isReady = useUsername(client); 

  if (!isReady) {
    return (
      <LoadingIndicator/>
    )
  }

  return (
    <div>
      <NavBar />
      <Router />
    </div>
  );
}

export default withApollo(App);
