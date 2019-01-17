import React from 'react';
import styles from '../../../Common/Common.scss';

const Button = props => {
  const { type, onClick, children, size, color, className, disabled } = props;

  let extendedClassName = `${className} btn ${
    size ? `btn-${size} ` : 'button '
  }`;
  switch (color) {
    case 'yellow':
      extendedClassName += styles.yellow_button;
      break;
    case 'red':
      extendedClassName += 'btn-danger';
      break;
    case 'green':
      extendedClassName += 'btn-success';
      break;
    case 'gray':
      extendedClassName += styles.gray_button;
      break;
    default:
      extendedClassName += 'btn-default';
      break;
  }
  return (
    <button
      type={type}
      onClick={onClick}
      className={extendedClassName}
      data-test={props['data-test']}
      disabled={disabled}
    >
      {children}
    </button>
  );
};

export default Button;
