import React from 'react';

import { theme } from '../../theme';
import { Spinner } from '../Spinner';

import { StyledButton } from './Button';

export const Button = props => {
  const { children, type, size, disabled, isLoading } = props;

  const { button } = theme;

  let colorValue;
  let backgroundColorValue;
  let boxShadowColorValue;

  if (button[type]) {
    colorValue = button[type].color;
    backgroundColorValue = button[type].backgroundColor;
    boxShadowColorValue = button[type].boxShadowColor;
  } else {
    colorValue = button.default.color;
    backgroundColorValue = button.default.backgroundColor;
    boxShadowColorValue = button.default.boxShadowColor;
  }

  const borderColorValue =
    type === 'secondary' ? 'black.secondary' : backgroundColorValue;

  const buttonHeight = size === 'large' ? 'lg' : 'sm';

  const paddingX = size === 'large' ? 'lg' : 'md';

  return (
    <StyledButton
      {...props}
      height={buttonHeight}
      px={paddingX}
      opacity={disabled ? '0.5' : undefined}
      color={colorValue}
      bg={backgroundColorValue}
      boxShadowColor={boxShadowColorValue}
      fontSize="button"
      fontWeight="bold"
      display="flex"
      justifyContent="center"
      alignItems="center"
      border={1}
      borderRadius="xs"
      borderColor={borderColorValue}
    >
      {children}
      {isLoading && <Spinner size={size} ml={18} />}
    </StyledButton>
  );
};

Button.defaultProps = {
  size: 'small',
  isLoading: false,
  disabled: false,
};
