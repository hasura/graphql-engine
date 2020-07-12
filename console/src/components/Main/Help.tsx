import React from 'react';
import { Link } from 'react-router';

import styles from './Main.scss';

export const Help: React.FC = () => {
  return (
    <div className={`${styles.headerRightNavbarBtn}`}>
      <Link to="/support/forums/">HELP</Link>
    </div>
  );
};
