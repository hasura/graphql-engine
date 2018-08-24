import React from 'react';
import RenderMessages from './RenderMessages';
import Textbox from './Textbox'
import "../App.css";

export default class RenderMessagesProxy extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      refetch: null
    }
  }

  // Set mutation callback. For instantly adding messages to state after mutation
  setMutationCallback = (mutationCallback) => {
    this.setState({
      ...this.state,
      mutationCallback
    })
  }

  render() {
    return (
      <div>
        <RenderMessages
          refetch={this.props.refetch}
          setRefetch={this.props.setRefetch}
          setMutationCallback={this.setMutationCallback}
          username={this.props.username}
        />
        <Textbox
          username={this.props.username}
          mutationCallback={this.state.mutationCallback}
        />
      </div>
    );
  }
}
