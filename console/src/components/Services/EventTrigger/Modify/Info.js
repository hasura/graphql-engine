import React from 'react';

const Info = ({
  triggerName,
  tableName,
  schemaName,
  triggerId,
  styles,
  triggerType,
}) => (
  <div className={styles.container}>
    <div className={styles.modifySection}>
      <h4 className={styles.modifySectionHeading}>Info</h4>
      <div className={styles.modifyWebhookCollapsed}>
        Trigger name: {triggerName}
        <br />
        Trigger ID: {triggerId}
        <br />
        Trigger type: {triggerType}
        <br />
        Table: {tableName}
        <br />
        Schema: {schemaName}
        <br />
      </div>
    </div>
  </div>
);

export default Info;
