import React from 'react';

import { theme } from '../../theme';
import Spinner from '../Spinner';

import { StyledButton } from './Button';

const Button = props => {
  const { children, type, size, disabled, isLoading } = props;

  const { button } = theme;

  const buttonHeight = size === 'small' ? 'sm' : size === 'large' && 'lg';

  const paddingX = size === 'small' ? 'md' : size === 'large' && 'lg';

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

  return (
    <StyledButton
      {...props}
      height={buttonHeight}
      px={paddingX}
      opacity={disabled && '0.5'}
      color={colorValue}
      bg={backgroundColorValue}
      boxShadowColor={boxShadowColorValue}
      fontSize="button"
      fontWeight="bold"
      display={'flex'}
      justifyContent={'center'}
      alignItems={'center'}
      border={1}
      borderRadius="xs"
      // In case of secondary button ~ black border
      borderColor={
        type === 'secondary' ? 'black.secondary' : backgroundColorValue
      }
    >
      {children}
      {isLoading && <Spinner size={size} />}
    </StyledButton>
  );
};

Button.defaultProps = {
  size: 'small',
  isLoading: false,
  disabled: false,
};

export default Button;
