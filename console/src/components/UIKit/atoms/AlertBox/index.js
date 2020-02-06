import React from 'react';
import PropTypes from 'prop-types';

import { theme } from '../../theme';

import { AlertBoxStyles } from './Alert.style';

const AlertBox = props => {
  const { children, type } = props;

  // Here I'm dynamically passing color values from theme object based on alert type.

  const backgroundColor = theme.alertBoxes[type]
    ? theme.alertBoxes[type].backgroundColor
    : theme.alertBoxes.default.backgroundColor;

  const borderColor = theme.alertBoxes[type]
    ? theme.alertBoxes[type].borderColor
    : theme.alertBoxes.default.borderColor;

  // ***************************** //

  return (
    <AlertBoxStyles {...props} bg={backgroundColor} borderColor={borderColor}>
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
};

// Default props for AlertBox ********** //

AlertBox.defaultProps = {
  type: 'success',
  width: 866,
  height: 'lg',
  borderLeft: 4,
  borderRadius: 'xs',
  boxShadow: 2,
};

// ***************************** //

export default AlertBox;
