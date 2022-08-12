import React from 'react';
import { Link } from 'react-router';

import styles from '../Main.scss';

export const Help = ({ isSelected }: { isSelected: boolean }) => {
  return (
    <Link to="/support/forums/">
      <div className={styles.headerRightNavbarBtn}>
        HELP
        {isSelected ? (
          <span
            className={styles.selected}
            style={{ width: '90%', marginLeft: '5%' }}
          />
        ) : null}
      </div>
    </Link>
  );
};
