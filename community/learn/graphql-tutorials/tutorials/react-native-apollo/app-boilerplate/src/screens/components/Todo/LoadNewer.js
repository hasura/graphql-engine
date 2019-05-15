import React from 'react';
import { TouchableOpacity, Text } from 'react-native'
import CenterSpinner from '../Util/CenterSpinner';

class LoadNewerButton extends React.Component {

  constructor(props) {
    super(props);
    this.state = {
      buttonText: 'New todos have arrived!',
      loading: false,
    };
  }

  render () {
    const { disabled, buttonText, loading } = this.state;
    const { styles, show } = this.props;
    if (!show) {
      return null;
    }
    return (
      <TouchableOpacity
        style={styles.banner}
        onPress={this.fetchNewerTodos}
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

export default LoadNewerButton;