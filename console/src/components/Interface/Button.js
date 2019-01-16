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
}) => {
  let extendedClassName = `${className} btn ${size ? `btn-${size} ` : ''}`;
  switch (color) {
    case 'yellow':
      extendedClassName += styles.yellow_button;
      break;
    case 'white':
      extendedClassName += 'btn-default';
      break;
    case 'red':
      extendedClassName += 'btn-danger';
      break;
    default:
      break;
  }
  return (
    <button
      type={type}
      onClick={onClick}
      className={extendedClassName}
      data-test={dataTest}
    >
      {children}
    </button>
  );
};

export default Button;
