import React from 'react';

const Info = ({ triggerName, tableName, schemaName, triggerId, styles }) => (
  <div className={`${styles.container} ${styles.borderBottom}`}>
    <div className={styles.modifySection}>
      <h4 className={styles.modifySectionHeading}>Info</h4>
      <div className={styles.infoContent}>
        <table className="table table-striped table-bordered">
          <thead />
          <tbody>
            <tr>
              <td>Trigger name</td>
              <td>{triggerName}</td>
            </tr>
            <tr>
              <td>Trigger ID</td>
              <td>{triggerId}</td>
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
