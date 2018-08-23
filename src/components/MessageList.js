import React from 'react';
import '../App.js';

export default class MessageList extends React.Component {
  render() {
    const { isNew } = this.props;
    const getStyleById = (id) => {
      const evenOrOdd = (id % 2 === 0) ? "Even" : "Odd";
      return isNew ? `newMessage${evenOrOdd}` : `message${evenOrOdd}`;
    }
    return (
      <div>
        {
          this.props.messages.map((m, i) => {
            return (
              <div key={m.id} className={getStyleById(i)}>
                { m.username + ": " + m.text }
              </div> 
            );
          })
        }
      </div>
    );
  }
};