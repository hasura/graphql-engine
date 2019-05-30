import React from 'react';
import Tooltip from '../../../Common/Tooltip/Tooltip';

const Info = ({ triggerName, tableName, schemaName, styles }) => (
  <div className={`${styles.container} ${styles.borderBottom}`}>
    <div className={styles.modifySection}>
      <h4 className={styles.modifySectionHeading}>
        Info{' '}
        <Tooltip message="Delete this trigger and create another one if you need to edit any of these fields" />
      </h4>
      <div className={styles.infoContent}>
        <table className="table table-striped table-bordered">
          <thead />
          <tbody>
            <tr>
              <td>Trigger name</td>
              <td>{triggerName}</td>
            </tr>
            <tr>
              <td>Table</td>
              <td>{tableName}</td>
            </tr>
            <tr>
              <td>Schema</td>
              <td>{schemaName}</td>
            </tr>
          </tbody>
        </table>
      </div>
    </div>
  </div>
);

export default Info;
