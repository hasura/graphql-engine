import { ApolloClient, HttpLink, InMemoryCache, split } from "@apollo/client";
import { GraphQLWsLink } from '@apollo/client/link/subscriptions';
import { createClient } from "graphql-ws";
import { getMainDefinition } from "@apollo/client/utilities";

const scheme = (proto) =>
  window.location.protocol === "https:" ? `${proto}s` : proto;

const splitter = ({ query }) => {
  const { kind, operation } = getMainDefinition(query) || {};
  const isSubscription =
    kind === "OperationDefinition" && operation === "subscription";
  return isSubscription;
};

const GRAPHQL_ENDPOINT = "realtime-poll.hasura.app";
const cache = new InMemoryCache();
const options = { reconnect: true };

const wsURI = `${scheme("ws")}://${GRAPHQL_ENDPOINT}/v1/graphql`;
const httpurl = `${scheme("https")}://${GRAPHQL_ENDPOINT}/v1/graphql`;

const wsLink = new GraphQLWsLink(createClient({ url: wsURI, connectionParams: { options } }));
const httpLink = new HttpLink({ uri: httpurl });
const link = split(splitter, wsLink, httpLink);
const client = new ApolloClient({ link, cache });
export default client;
