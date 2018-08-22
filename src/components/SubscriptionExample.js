import React from 'react';
import { Query } from 'react-apollo';
import gql from 'graphql-tag';
import RenderMessages from './RenderMessages';
import '../App.js';

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
    this.state = {};
  } 
  
  render() {
    return (
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
		    		<div className="App">
		    			<RenderMessages messages={data.message} refetch={refetch}/>
		    		</div>
		    	)
		    }}
		  </Query>
 	 	)
  }
}
