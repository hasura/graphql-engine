import React, { useState } from 'react';
import styles from '../Common.scss';

interface HiddenMoreProps {
  title: string;
  more: React.ReactNode;
  defaultExpanded?: boolean;
}

const HiddenMore: React.VFC<HiddenMoreProps> = ({
  title,
  more,
  defaultExpanded = false,
}) => {
  const [isExpanded, setIsExpanded] = useState(defaultExpanded);

  const toggle = () => setIsExpanded(prev => !prev);

  return (
    <div>
      <div
        className={`${styles.display_flex} ${styles.add_mar_bottom} ${styles.cursorPointer} ${styles.hiddenMoreWidth}`}
        onClick={toggle}
      >
        <i
          className={`fa fa-chevron-${isExpanded ? 'down' : 'right'} ${
            styles.add_mar_right_small
          }`}
          aria-hidden="true"
        />
        <b>{title}</b>
      </div>
      {isExpanded && more}
    </div>
  );
};

export default HiddenMore;
