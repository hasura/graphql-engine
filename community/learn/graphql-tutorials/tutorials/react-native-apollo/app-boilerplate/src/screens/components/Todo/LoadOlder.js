import React from 'react';
import { TouchableOpacity, Text } from 'react-native'
import CenterSpinner from '../Util/CenterSpinner';

class LoadOlderButton extends React.Component {

  constructor(props) {
    super(props);
    this.state = {
      buttonText: 'Load more todos',
      disabled: false,
      loading: false,
    };
  }

  render () {
    const { disabled, buttonText, loading } = this.state;
    const { styles } = this.props;
    return (
      <TouchableOpacity
        style={styles.pagination}
        onPress={this.fetchOlderTodos}
        disabled={disabled}
      > 
        {
          loading ?
          <CenterSpinner /> :
          <Text style={styles.buttonText}>
            {buttonText}
          </Text>
        }
      </TouchableOpacity> 
    )
  }
}

export default LoadOlderButton;