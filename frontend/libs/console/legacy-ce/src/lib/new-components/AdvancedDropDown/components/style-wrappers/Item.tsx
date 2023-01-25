import clsx from 'clsx';
import React from 'react';
import * as styles from './item-styles';

export const Item: React.FC<{
  dangerous?: boolean;
  selectable?: boolean;
}> = ({ children, dangerous, selectable = false }) => (
  <div
    className={clsx(
      styles.twBaseStyle,
      selectable && styles.twSelectableItem,
      styles.twDefault,
      dangerous && styles.twDangerous
    )}
  >
    {children}
  </div>
);
