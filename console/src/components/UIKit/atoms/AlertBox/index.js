import React from 'react';
import PropTypes from 'prop-types';

import { theme } from '../../theme';
import Icon from '../../Icons';

import { AlertBoxStyles } from './Alert.style';
import { Text } from '../Typography';

const AlertBox = props => {
  const { children, type } = props;

  /* Here I'm dynamically passing color values from theme object based on alert type.
   *  - backgroundColor
   *  - borderColor
   *  If the alert type is out of the range then I'll assign default color values.
   */

  const backgroundColor = theme.alertBoxes[type]
    ? theme.alertBoxes[type].backgroundColor
    : theme.alertBoxes.default.backgroundColor;

  const borderColor = theme.alertBoxes[type]
    ? theme.alertBoxes[type].borderColor
    : theme.alertBoxes.default.borderColor;

  // ***************************** //

  return (
    <AlertBoxStyles {...props} bg={backgroundColor} borderColor={borderColor}>
      <Icon iconType={type} />
      <Text as="span" px="md" fontWeight="medium" fontSize="p">
        {type}
      </Text>
      {children}
    </AlertBoxStyles>
  );
};

// PropTypes for AlertBox *************** //

AlertBox.propTypes = {
  type: PropTypes.string,
  width: PropTypes.number,
  height: PropTypes.string,
  backgroundColor: PropTypes.string,
  borderLeft: PropTypes.number,
  borderColor: PropTypes.string,
  borderRadius: PropTypes.string,
  boxShadow: PropTypes.number,
  display: PropTypes.string,
  alignItems: PropTypes.string,
  pl: PropTypes.string,
  color: PropTypes.string,
};

// Default props for AlertBox ********** //

AlertBox.defaultProps = {
  type: 'success',
  width: 866,
  height: 'lg',
  borderLeft: 4,
  borderRadius: 'xs',
  boxShadow: 2,
  display: 'flex',
  alignItems: 'center',
  pl: 'md',
  color: 'black.text',
};

// ***************************** //

export default AlertBox;
