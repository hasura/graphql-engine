import React from 'react';
import { Query } from 'react-apollo';
import gql from 'graphql-tag';
import RenderMessages from './RenderMessages';
import '../App.js';
import '../App.css';

const fetchMessages = gql`
  query {
    message (
      order_by: timestamp_asc
    ) {
      id
      text
      username
      timestamp
    }
  }
`;

export default class ChatComponent extends React.Component {

  constructor() {
    super();
    this.state = {
      isLoggedIn: false,
      username:""
    };
  } 

  render() {
    return (
      <div className="App">
        {
          !this.state.isLoggedIn ? (
            <div className="login">
              <div>
                <input type="text" id="username" placeholder="Username" value={this.state.username} onChange={e => {
                  this.setState({username:e.target.value});
                }}/>
              <button onClick={e => {
                e.preventDefault();
                if(this.state.username != ""){
                  this.setState({isLoggedIn: true});
                }
              }} 
            >
              Enter
            </button> 
          </div>
        </div>
          ) : (
            <Query
              query={fetchMessages}
            >
              {({ data, loading, error, refetch }) => {
                if (loading) {
                  return (<h2> Loading </h2>);
                }
                if (error) {
                  return (JSON.stringify(error, null, 2));
                }
                if (!this.state.refetch) {
                  this.setState({refetch});
                }
                return (
                  <RenderMessages messages={data.message} refetch={refetch}/>
                )
              }}
            </Query>
          )
        }
      </div>
    )
  }
}
