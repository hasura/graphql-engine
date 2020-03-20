import React from 'react';

import { theme } from '../../theme';
import { Icon } from '../Icon';
import { StyledAlertBox } from './AlertBox';
import { Text } from '../Typography';

const alertBoxWidth = 866;

export const AlertBox = props => {
  const { children, type } = props;

  const backgroundColorValue = theme.alertBox[type]
    ? theme.alertBox[type].backgroundColor
    : theme.alertBox.default.backgroundColor;

  const borderColorValue = theme.alertBox[type]
    ? theme.alertBox[type].borderColor
    : theme.alertBox.default.borderColor;

  let alertMessage;

  if (children) {
    alertMessage = children;
  } else {
    alertMessage = theme.alertBox[type]
      ? theme.alertBox[type].message
      : theme.alertBox.default.message;
  }

  return (
    <StyledAlertBox
      width={alertBoxWidth}
      bg={backgroundColorValue}
      borderRadius="xs"
      fontSize="p"
      borderLeft={4}
      borderColor={borderColorValue}
      boxShadow={2}
      height="lg"
      pl="md"
      display="flex"
      alignItems="center"
      color="black.text"
      {...props}
    >
      <Icon type={type} />
      {type && (
        <Text as="span" pl="md" fontWeight="medium">
          {type}
        </Text>
      )}
      <Text pl="md">{alertMessage}</Text>
    </StyledAlertBox>
  );
};
