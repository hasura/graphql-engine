import React from 'react';
import { TouchableOpacity, Text } from 'react-native'
import CenterSpinner from './CenterSpinner';

class LoadNewerButton extends React.Component {

  constructor(props) {
    super(props);
    this.state = {
      buttonText: 'New tasks have arrived!',
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
        style={styles.pagination}
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