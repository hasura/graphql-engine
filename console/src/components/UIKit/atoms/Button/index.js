import React from 'react';
import PropTypes from 'prop-types';

import { ButtonStyles } from './Button.style';

const Button = props => {
  const { children, type, size } = props;

  // ************************* //

  // Button Sizes ~ height & padding (x-axis)

  const paddingX = size === 'small' ? 'md' : size === 'large' && 'lg';

  const buttonHeight = size === 'small' ? 'sm' : size === 'large' && 'lg';

  // Button Types ~ background & font color.

  let backgroundColor;

  switch (type) {
    case 'primary':
      backgroundColor = 'yellow.primary';
      break;
    case 'secondary':
      backgroundColor = 'white';
      break;
    case 'success':
      backgroundColor = 'green.primary';
      break;
    case 'danger':
      backgroundColor = 'red.primary';
      break;
    case 'warning':
      backgroundColor = 'orange.primary';
      break;
    case 'info':
      backgroundColor = 'blue.primary';
      break;
    default:
      backgroundColor = 'yellow.primary';
  }

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
    >
      {children}
    </ButtonStyles>
  );
};

// PropTypes for Button *************** //

Button.propTypes = {
  size: PropTypes.oneOf(['small', 'large']),
  type: PropTypes.string,
  borderRadius: PropTypes.string,
  border: PropTypes.number,
  fontSize: PropTypes.string,
  fontWeight: PropTypes.string,
};

// Default props for button ********** //

Button.defaultProps = {
  type: 'primary',
  fontSize: 'button',
  fontWeight: 'bold',
  size: 'small',
  borderRadius: 'xs',
  border: 1,
};

// ***************************** //

export default Button;
