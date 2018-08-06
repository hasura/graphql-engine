import React, { Component } from 'react';
import '../node_modules/graphiql/graphiql.css';

import GraphiQL from 'graphiql';
import {query, variables} from './graphiql-vars-live';

import { parse } from 'graphql';
import { execute } from 'apollo-link';
import { WebSocketLink } from "apollo-link-ws";
import { SubscriptionClient } from "subscriptions-transport-ws";

class App extends Component {

  render() {

      const GRAPHQL_ENDPOINT = 'ws://' + window.location.host + "/v1alpha1/graphql";

      const client = new SubscriptionClient(
          GRAPHQL_ENDPOINT, {}
      );

      const link = new WebSocketLink(client);
      const fetcher = (operation) => {
          operation.query = parse(operation.query);
          return execute(link, operation);
      };

      var content = <GraphiQL fetcher={fetcher} query={query} variables={variables}/>;

      return (
          <div className="react-container-graphql">
              {content}
          </div>
      );
  }
}

export default App;
