import React from 'react';

import styles from '../Common.scss';

/*
  This is a Button HOC that takes all the props supported by <button>
  - color(default: white): color of the button; currently supports yellow, red, green, gray and white
  - size: size of the button; currently supports xs (extra small), sm(small)
  - className: although you can provide any CSS classname, it is recommended to use only the positioning related classes
               and not the ones that change the appearance (color, font, size) of the button
*/

export interface ButtonProps extends React.ComponentProps<'button'> {
  size: string;
  color: 'yellow' | 'red' | 'green' | 'gray' | 'white' | 'black';
}

const Button: React.FC<ButtonProps> = props => {
  const { children, size, color, className, type = 'button' } = props;
  let extendedClassName = `${className || ''} btn ${
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
    <button {...props} className={extendedClassName} type={type}>
      {children}
    </button>
  );
};

export default Button;
