import React from "react"
import { Router } from "@reach/router"
import { login, logout, isAuthenticated, getProfile } from "../utils/auth"
import { Link } from "gatsby"
import Playlist from "../components/Playlist";

import ApolloClient from 'apollo-client';
import { HttpLink } from 'apollo-link-http';
import { InMemoryCache } from 'apollo-cache-inmemory';

const createApolloClient = (authToken) => {
  return new ApolloClient({
    link: new HttpLink({
      uri: 'http://localhost:8080/v1/graphql',
      headers: {
        Authorization: `Bearer ${authToken}`
        // 'X-Hasura-Admin-Secret': 'myadminsecretkey'
      }
    }),
    cache: new InMemoryCache(),
  });
 };

const Home = ({ user }) => {
  return <p>Hi, {user.name ? user.name : "friend"}!</p>
}

const Account = ({ data }) => {
  if (!isAuthenticated()) {
    login()
    return <p>Redirecting to login...</p>
  }

  const user = getProfile();
  const client = createApolloClient(user.idToken);

  return [
      <nav key="links">
        <Link to="/account">Home</Link>{" "}
        <Link to="/account/playlist">Playlist</Link>{" "}
        <a
          href="#logout"
          onClick={e => {
            logout()
            e.preventDefault()
          }}
        >
          Log Out
        </a>
      </nav>,
      <Router key="router">
        <Home path="/account" user={user} />
        <Playlist client={client} path="/account/playlist" />
      </Router>
  ];
}

export default Account;

