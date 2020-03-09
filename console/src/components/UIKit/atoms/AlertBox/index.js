import React from 'react';

import { theme } from '../../theme';
import { Icon } from '../Icon';

import { AlertBoxStyles } from './Alert.style';
import { Text } from '../Typography';

export const AlertBox = props => {
  const { children, type } = props;

  /* Color values dynamically assigned from theme object based on alert type.
   *  - backgroundColor
   *  - borderColor
   *  If the alert type is out of the range then default color value is assigned.
   */

  const backgroundColor = theme.alertBox[type]
    ? theme.alertBox[type].backgroundColor
    : theme.alertBox.default.backgroundColor;

  const borderColor = theme.alertBox[type]
    ? theme.alertBox[type].borderColor
    : theme.alertBox.default.borderColor;

  // Default message for AlertBox

  let alertMessage;

  if (children) {
    alertMessage = children;
  } else {
    alertMessage = theme.alertBox[type]
      ? theme.alertBox[type].message
      : theme.alertBox.default.message;
  }

  // ***************************** //

  return (
    <AlertBoxStyles {...props} bg={backgroundColor} borderColor={borderColor}>
      <Icon type={type} />
      {/* Alert Type */}
      {type && (
        <Text as="span" pl="md" fontWeight="medium">
          {type}
        </Text>
      )}
      {/* Alert Message ~ {children} */}
      <Text pl="md">{alertMessage}</Text>
    </AlertBoxStyles>
  );
};

AlertBox.defaultProps = {
  width: 866,
  height: 'lg',
  borderLeft: 4,
  borderRadius: 'xs',
  fontSize: 'p',
  boxShadow: 2,
  display: 'flex',
  alignItems: 'center',
  pl: 'md',
  color: 'black.text',
};
