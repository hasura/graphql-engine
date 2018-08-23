import React from 'react';
import RenderMessages from './RenderMessages';
import { ApolloConsumer } from 'react-apollo';
import gql from 'graphql-tag';
import Textbox from './Textbox'

export default class RenderMessagesProxy extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      refetch: null
    }
  }

  render() {
    return (
      <div>
        <ApolloConsumer>
          {
            (client) => {
              return (
                <RenderMessages
                  client={client}
                  refetch={this.props.refetch}
                  setRefetch={this.props.setRefetch}
                />
              );
            }
          }
        </ApolloConsumer>
        <Textbox
          username={this.props.username}
        />
      </div>
    );
  }
}
