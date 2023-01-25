import React from 'react';
import styles from '../Common/Common.module.scss';

const AccessDenied = ({ alignCenter = true }) => {
  return (
    <div
      className={styles.display_flex}
      style={{
        ...(alignCenter && { justifyContent: 'center' }),
        marginTop: '50px',
      }}
    >
      <h4>
        You don't have enough permissions to view this section. Ask the project
        owner to grant you the required privileges.
      </h4>
    </div>
  );
};

export default AccessDenied;
