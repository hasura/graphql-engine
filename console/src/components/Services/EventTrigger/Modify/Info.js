import React from 'react';

import { ToolTip } from '../../../UIKit/atoms';

const Info = ({ triggerName, tableName, schemaName, styles }) => (
  <div className={`${styles.container} ${styles.borderBottom}`}>
    <div className={styles.modifySection}>
      <h4 className={styles.modifySectionHeading}>
        Info
        <ToolTip
          message="Delete this trigger and create another one if you need to edit any of these fields"
          ml="sm"
        />
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
