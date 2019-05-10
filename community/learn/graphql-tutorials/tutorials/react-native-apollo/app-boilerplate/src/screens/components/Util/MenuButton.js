import React from 'react';
import Icon from 'react-native-vector-icons/MaterialIcons';
import { TouchableOpacity } from 'react-native';

const MenuButton = ({ onPress }) => {

  return (
    <TouchableOpacity style={{marginLeft: 20}} onPress={onPress}>
      <Icon
        name="menu"
        size={26}
        style={{ marginBottom: -3 }}
      />
    </TouchableOpacity>
  )
}

export default MenuButton;