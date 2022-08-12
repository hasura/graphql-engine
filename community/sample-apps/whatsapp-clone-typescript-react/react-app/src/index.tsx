import { MuiThemeProvider, createMuiTheme } from '@material-ui/core/styles'
import React from 'react';
import { Suspense } from 'react'
import ReactDOM from 'react-dom';
import { ApolloProvider } from 'react-apollo-hooks';
import './index.css';
import App from './App';
import apolloClient from './apollo-client'
import * as serviceWorker from './serviceWorker';

const theme = createMuiTheme({
  palette: {
    primary: { main: '#2c6157' },
    secondary: { main: '#6fd056' },
  },
  typography: {
    useNextVariants: true,
  },
})

ReactDOM.render(
  <MuiThemeProvider theme={theme}>
    <ApolloProvider client={apolloClient}>
      <Suspense fallback={null}>
        <App />
      </Suspense>
    </ApolloProvider>
  </MuiThemeProvider>
, document.getElementById('root'));

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: http://bit.ly/CRA-PWA
serviceWorker.unregister();
