import React from 'react';
import RenderMessages from './RenderMessages';
import { ApolloConsumer } from 'react-apollo';

export default class RenderMessagesProxy extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      refetch: null
    }
  }

  render() {
    return (
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
    );
  }
}


