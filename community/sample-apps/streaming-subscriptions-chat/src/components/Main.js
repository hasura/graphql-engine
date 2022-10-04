import React, { useEffect, useState } from 'react';
import { ThemeProvider } from 'styled-components';
import { ApolloConsumer, gql, useMutation } from '@apollo/client';
import generateUsername from 'project-name-generator';
import styled from 'styled-components';

import { darkTheme, lightTheme } from '../styles/theme';
import Chat from './Chat';
// import LandingPage from './LandingPage';
import '../App.css';

const StyledApp = styled.div`
  background: ${({ theme }) => theme.colors.background};
  min-height: 100vh;
  height: 100vh;
  width: 100%;
  padding: 0 24px;
  overflow-y: auto;

  ::-webkit-scrollbar {
    background: ${({ theme }) =>
      theme.name === 'dark' ? '#1c262f' : '#FAFAFA'};
    width: 12px;
  }

  ::-webkit-scrollbar-thumb {
    background: ${({ theme }) =>
      theme.name === 'dark' ? '#394e60' : '#A6B6C4'};
    border-radius: 80px;
    width: 12px;
  }

  @media (min-width: 1250px) {
    background-image: url('https://graphql-engine-cdn.hasura.io/assets/main-site/hasura_sf_illus.png');
    background-repeat: no-repeat;
    background-position: 95% 10%;
  }

  @media (min-width: 1250px) and (max-width: 1450px) {
    background-position: 92% 10%;
    background-size: 250px;
  }

  @media (max-width: 590px) {
    padding: 0;
  }
`;

const addUser = gql`
  mutation ($username: String!) {
    insert_user_one(
      object: { username: $username }
      on_conflict: { constraint: user_username_key, update_columns: [] }
    ) {
      id
      username
    }
  }
`;

export default function Main(props) {
  const [isLoggedIn, setIsLoggedIn] = useState(true);
  const [username, setUsername] = useState(generateUsername().dashed);
  const [userId, setUserId] = useState(null);

  const [isDarkThemeActive, toggleActiveTheme] = useState(true);

  const [newUserHandler] = useMutation(addUser, {
    variables: {
      username,
    },
    onCompleted: (data) => {
      if (data.insert_user_one?.id) {
        setUserId(data.insert_user_one.id);
      }
    },
  });

  // check usernme and  perform login
  const login = (id) => {
    setIsLoggedIn(true);
    setUserId(id);
  };

  const toggleDarkTheme = () => {
    toggleActiveTheme(!isDarkThemeActive);

    window.localStorage.setItem(
      'isDarkThemeActive',
      JSON.stringify(!isDarkThemeActive)
    );
  };

  // const retrieveActiveTheme = () => {
  //   const isDarkThemeActive = JSON.parse(
  //     window.localStorage.getItem('isDarkThemeActive')
  //   );

  //   // toggleDarkTheme({ isDarkThemeActive });
  // };

  // useEffect(() => {
  //   retrieveActiveTheme();
  // }, []);

  const currentActiveTheme = isDarkThemeActive ? darkTheme : lightTheme;

  useEffect(() => {
    if (!userId) {
      newUserHandler();
    }
  }, [newUserHandler, userId]);

  return (
    <ThemeProvider theme={currentActiveTheme}>
      <StyledApp className="app">
        {!userId ? (
          <div>Loading</div>
        ) : (
          // <LandingPage
          //   setUsername={setUsername}
          //   login={login}
          //   username={username}
          // />
          <ApolloConsumer>
            {(client) => {
              return (
                <Chat
                  userId={userId}
                  username={username}
                  client={client}
                  isDarkThemeActive={isDarkThemeActive}
                  toggleDarkTheme={toggleDarkTheme}
                />
              );
            }}
          </ApolloConsumer>
        )}
      </StyledApp>
    </ThemeProvider>
  );
}
