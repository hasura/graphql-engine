import React from 'react';
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
        <i
          className={`fa fa-chevron-${isExpanded ? 'down' : 'right'} ${
            styles.add_mar_right_small
          }`}
          aria-hidden="true"
        />
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
