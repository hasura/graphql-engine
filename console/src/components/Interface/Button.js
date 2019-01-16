import React from 'react';
import styles from '../Common/Common.scss';

const Button = ({
  type,
  onClick,
  children,
  size,
  color,
  dataTest,
  className,
  disabled,
}) => {
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
    default:
      extendedClassName += 'btn-default';
      break;
  }
  return (
    <button
      type={type}
      onClick={onClick}
      className={extendedClassName}
      data-test={dataTest}
      disabled={disabled}
    >
      {children}
    </button>
  );
};

export default Button;
