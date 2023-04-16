import clsx from 'clsx';
import React from 'react';
import * as styles from './item-styles';

export const Item: React.FC<{
  dangerous?: boolean;
  link?: boolean;
  selectable?: boolean;
}> = ({ children, dangerous, link, selectable = false }) => (
  <div
    className={clsx(
      styles.twBaseStyle,
      selectable && styles.twSelectableItem,
      styles.twDefault,
      dangerous && styles.twDangerous,
      link && styles.twLink
    )}
  >
    {children}
  </div>
);
