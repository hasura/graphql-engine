import React from 'react';
import '../App.css';
import { Mutation } from 'react-apollo';
import gql from 'graphql-tag'

const addUser = gql`
  mutation ($username: String!) {
    insert_user (
      objects: [{
        username: $username
      }]
    ) {
      returning {
        id
        username
      }
    }
  }
`;

const LoginComponent = (props) => {
  return (
    <div className="loginWrapper">
      <h2 className="loginHeading"> Welcome to sample chat app made with Hasura GraphQL Engine </h2>
      <div className="login">
        <Mutation
          mutation={addUser}
          variables={{
            username: props.username
          }}
          onCompleted={(data) => {
            props.login(data.insert_user.returning[0].id);
          }}
          onError={() => {
            props.setUsername('');
          }}
        >
          {
            (insert_user, { data, error, loading}) => {
              if (loading) { return "Loading"; }
              const errorMessage = error ? 
                  <div className="errorMessage"> Try again with a different username </div> :
                  null;
              return (
                <div>
                  { errorMessage}
                  <form>
                    <input
                      type="text"
                      id="username"
                      className="loginTextbox"
                      placeholder="Username"
                      autoFocus={true}
                      value={props.username}
                      onChange={(e) => props.setUsername(e.target.value)}
                    />
                    <button
                      className="loginButton"
                      type="submit"
                      onClick={(e) => {
                        e.preventDefault();
                        if (props.username.match(/^[a-z0-9_-]{3,15}$/g)) {
                          insert_user();
                        } else {
                          alert("Invalid username. Spaces and special characters not allowed. Please try again");
                          props.setUsername('');
                        }
                      }}
                    >
                      Enter
                    </button>
                  </form>
                </div>
              );
            }
          }
        </Mutation>
      </div>
    </div>
  );
};

export default LoginComponent;
