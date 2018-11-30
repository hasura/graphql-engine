import React from 'react';

const Info = ({ triggerName, tableName, schemaName, triggerId, styles }) => (
  <div className={styles.container}>
    <div className={styles.modifySection}>
      <h4 className={styles.modifySectionHeading}>Info</h4>
      <div className={styles.infoContent}>
        {/*        Trigger name: {triggerName}
        <br />
        Trigger ID: {triggerId}
        <br />
        Trigger type: {triggerType}
        <br />
        Table: {tableName}
        <br />
        Schema: {schemaName}
        <br /> */}
        <KeyValues keyname="Trigger Name" value={triggerName} styles={styles} />
        <KeyValues keyname="Trigger ID" value={triggerId} styles={styles} />
        <KeyValues keyname="Table" value={tableName} styles={styles} />
        <KeyValues keyname="Schema" value={schemaName} styles={styles} />
      </div>
    </div>
  </div>
);

const KeyValues = props => {
  console.log(props);
  const { keyname, value, styles } = props;
  return (
    <div className={styles.info}>
      <p> {keyname}: </p>
      <input
        className={`${styles.input} form-control ${styles.add_mar_right} ${
          styles.modifyHeaderCollapsedInput
        }`}
        value={value}
        disabled
      />
    </div>
  );
};

export default Info;
