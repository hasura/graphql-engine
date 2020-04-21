import React, { Component } from 'react';
import logo from './img/logo-white.svg';
import './App.css';
import { ApolloProvider } from 'react-apollo';
import client from './apollo';
import Poll from './Poll';
import { getUserId } from './session';
import { GraphQL } from './GraphQL';
import { Users } from './Users';


class App extends Component {
  constructor(props) {
    super(props);
    this.state = { loading: true, userId: '' };
  }

  componentDidMount() {
    getUserId().then((userId) => {
      this.setState({ loading: false, userId });
    });
  }

  render() {
    // if (this.state.loading) return <p>Loading...</p>;
    return (
      <ApolloProvider client={client}>
        <div className="App">

          <header className="App-header displayFlex">
            <div className="container displayFlex">
              <img src={logo} className="App-logo" alt="logo" />
              <h1 className="App-title">Realtime Poll</h1>
            </div>
          </header>

          <Users />

          <Poll userId={this.state.userId} />

          <GraphQL />

          <footer className="App-footer displayFlex">
            <div className="container hasura-logo">
              <a href="https://hasura.io" target="_blank" rel="noopener noreferrer">
                <img className="hasura-logo" alt="hasura logo" src="https://graphql-engine-cdn.hasura.io/img/powered_by_hasura_black_200px.png" />
              </a>
              &nbsp; | &nbsp;
              <a href="/console" target="_blank">
                Backend
              </a>
              &nbsp; | &nbsp;
              <a href="https://github.com/hasura/graphql-engine/tree/master/community/sample-apps/realtime-poll" target="_blank" rel="noopener noreferrer">
                Source
              </a>
              <div className="footer-small-text"><span>(The database resets every 24 hours)</span></div>
            </div>
          </footer>

        </div>
      </ApolloProvider>
    );
  }
}

export default App;
