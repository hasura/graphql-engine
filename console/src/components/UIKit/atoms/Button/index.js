import React from 'react';
import PropTypes from 'prop-types';

import { theme } from '../../theme';

import { ButtonStyles } from './Button.style';

const Button = props => {
  const { children, type, size, disabled } = props;

  // ************************* //

  // Button Sizes ~ height & padding (x-axis)

  const paddingX = size === 'small' ? 'md' : size === 'large' && 'lg';

  const buttonHeight = size === 'small' ? 'sm' : size === 'large' && 'lg';

  // Background Color is dynamically assigned from theme object based on button type. If the type is out of specified range then default color is assigned.

  const backgroundColor = theme.buttons[type]
    ? theme.buttons[type].backgroundColor
    : theme.buttons.default.backgroundColor;

  // ************************* //

  return (
    <ButtonStyles
      {...props}
      // **** Props ~ based on button size **** //
      height={buttonHeight}
      px={paddingX}
      // **** Props ~ based on button type **** //
      bg={backgroundColor}
      // secondary button ~ black border
      borderColor={type === 'secondary' ? 'black.secondary' : backgroundColor}
      // **** Disabled State **** //
      opacity={disabled && '0.5'}
    >
      {children}
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
};

// Default props for button ********** //

Button.defaultProps = {
  type: 'primary',
  fontSize: 'button',
  fontWeight: 'bold',
  size: 'small',
  borderRadius: 'xs',
  border: 1,
  disabled: false,
};

// ***************************** //

export default Button;
