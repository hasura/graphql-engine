import React, { ComponentProps } from 'react';
import styles from '../Common.scss';

interface RemoveIconProps extends ComponentProps<'i'> {}
const RemoveIcon: React.FC<RemoveIconProps> = ({
  className = '',
  ...props
}) => (
  <i
    className={`${styles.fontAwosomeClose} fa-lg fa fa-times ${
      className || ''
    }`}
    {...props}
  />
);

export default RemoveIcon;
