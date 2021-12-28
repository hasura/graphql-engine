import React, { useState } from 'react';

import styles from '../RESTStyles.scss';

type CollapsibleToggleProps = {
  state: Record<string, any>[];
  properties: string[];
  title: string;
};

const CollapsibleToggle: React.FC<CollapsibleToggleProps> = ({
  title,
  state,
  properties,
  children,
}) => {
  const [isOpen, setIsOpen] = useState(true);

  const toggleHandler = () => setIsOpen(prev => !prev);

  return (
    <div
      className={`${styles.collapsibleWrapper} ${
        !isOpen ? styles.collapsedWrapper : ''
      }`}
    >
      <div
        className={styles.collapsibleToggle}
        onClick={toggleHandler}
        role="button"
        tabIndex={0}
      >
        <span className={styles.collapsibleIndicatorWrapper}>
          <i
            className={`fa fa-chevron-right ${styles.collapsibleIndicator} ${
              isOpen && styles.collapsibleIndicatorOpen
            }`}
          />
        </span>

        <span className={styles.titleWrapper}>
          <div className={styles.defaultCollapsibleTitle}>{title}</div>
        </span>
      </div>
      <br />

      {isOpen ? (
        <>{children}</>
      ) : (
        <div className={styles.collapsible_info_container}>
          {state?.map((stateVar, index) => (
            <div className={styles.collapsed_text_container}>
              <div className={styles.collapsed_text}>
                {`${stateVar[properties[0]]}  :  ${stateVar[properties[1]]}`}
              </div>
              {index !== state?.length - 1 ? (
                <p className={styles.collapsed_separator}>|</p>
              ) : null}
            </div>
          ))}
        </div>
      )}
    </div>
  );
};

export default CollapsibleToggle;
