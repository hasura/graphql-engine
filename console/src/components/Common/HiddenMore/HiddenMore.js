import React from 'react';

import { Icon } from '../../UIKit/atoms';
import styles from '../Common.scss';

const HiddenMore = ({ title, more, expanded = false }) => {
  const [isExpanded, setIsExpanded] = React.useState(expanded);

  const toggle = () => setIsExpanded(!isExpanded);

  const getTitle = () => {
    return (
      <div
        className={`${styles.display_flex} ${styles.add_mar_bottom} ${styles.cursorPointer} ${styles.hiddenMoreWidth}`}
        onClick={toggle}
      >
        <Icon type={isExpanded ? 'down' : 'right'} mr="xs" />
        <b>{title}</b>
      </div>
    );
  };

  return (
    <div>
      {getTitle()}
      {isExpanded && more}
    </div>
  );
};

export default HiddenMore;
