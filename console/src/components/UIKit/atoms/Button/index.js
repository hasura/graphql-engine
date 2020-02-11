import React from 'react';
import PropTypes from 'prop-types';

import { theme } from '../../theme';
import Spinner from '../Spinner';

import { ButtonStyles } from './Button.style';

const Button = props => {
  const { children, type, size, disabled, color, bg, isLoading } = props;

  // ************************* //

  // Button Sizes ~ height & padding (x-axis)

  const paddingX = size === 'small' ? 'md' : size === 'large' && 'lg';

  const buttonHeight = size === 'small' ? 'sm' : size === 'large' && 'lg';

  /*  Button ~ color and background
   *  bg / color ~ by passing bg / color prop we can override default color values.
   *  Default color values are dynamically assigned from theme object based on button type. If the type is out of specified range then default color is assigned.
   */

  let backgroundColorValue;
  let colorValue;

  if (bg) {
    backgroundColorValue = bg;
  } else {
    backgroundColorValue = theme.buttons[type]
      ? theme.buttons[type].backgroundColor
      : theme.buttons.default.backgroundColor;
  }

  if (color) {
    colorValue = color;
  } else {
    colorValue = theme.buttons[type]
      ? theme.buttons[type].color
      : theme.buttons.default.color;
  }

  // ************************* //

  return (
    <ButtonStyles
      {...props}
      // **** Props ~ based on button size **** //
      height={buttonHeight}
      px={paddingX}
      // **** Props ~ based on button type **** //
      bg={backgroundColorValue}
      // secondary button ~ black border
      borderColor={
        type === 'secondary' ? 'black.secondary' : backgroundColorValue
      }
      // **** Disabled State **** //
      opacity={disabled && '0.5'}
      color={colorValue}
    >
      {children}
      {/* Spinner ~ Loading State */}
      {isLoading && <Spinner size={size} />}
    </ButtonStyles>
  );
};

// PropTypes for Button *************** //

Button.propTypes = {
  children: PropTypes.node,
  size: PropTypes.oneOf(['small', 'large']),
  type: PropTypes.string,
  borderRadius: PropTypes.string,
  border: PropTypes.number,
  fontSize: PropTypes.string,
  fontWeight: PropTypes.string,
  disabled: PropTypes.bool,
  color: PropTypes.string,
  display: PropTypes.string,
  justifyContent: PropTypes.string,
  alignItems: PropTypes.string,
  isLoading: PropTypes.bool,
};

// Default props for button ********** //

Button.defaultProps = {
  fontSize: 'button',
  fontWeight: 'bold',
  size: 'small',
  borderRadius: 'xs',
  border: 1,
  display: 'flex',
  justifyContent: 'center',
  alignItems: 'center',
  isLoading: false,
  disabled: false,
};

// ***************************** //

export default Button;
