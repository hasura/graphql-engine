import { useState } from 'react';
import { ApolloConsumer } from '@apollo/client';
import Chat from './Chat';
import LandingPage from './LandingPage';
import '../App.css';

export default function Main() {
  const [isLoggedIn, setIsLoggedIn] = useState(false);
  const [username, setUsername] = useState('');
  const [userId, setUserId] = useState(null);

  // check usernme and  perform login
  const login = (id) => {
    setIsLoggedIn(true);
    setUserId(id);
  };

  return (
    <div className="app">
      {!isLoggedIn ? (
        <LandingPage
          setUsername={setUsername}
          login={login}
          username={username}
        />
      ) : (
        <ApolloConsumer>
          {(client) => {
            return <Chat userId={userId} username={username} client={client} />;
          }}
        </ApolloConsumer>
      )}
    </div>
  );
}
