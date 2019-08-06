import React, { useState } from 'react';
import styles from '../RemoteSchema.scss';

const GraphQLType = ({ typeName, fields }) => {
  const [isExpanded, setExpansion] = useState(true);
  const expandType = () => setExpansion(true);
  const collapseType = () => setExpansion(false);

  const getTypeName = () => {
    const onClick = isExpanded ? collapseType : expandType;
    return (
      <div
        className={`${styles.display_flex} ${styles.add_mar_bottom_mid}`}
        onClick={onClick}
      >
        <i
          className={`fa fa-chevron-${isExpanded ? 'down' : 'right'} ${
            styles.add_mar_right_mid
          }`}
        />
        <b>{typeName}</b>
      </div>
    );
  };

  const getFieldsCheckbox = () => {
    if (!isExpanded) return null;
    return (
      <div
        className={`${styles.remoteSchemaPermType} ${styles.add_padding} ${
          styles.add_mar_left
        }`}
      >
        {Object.keys(fields).map(f => {
          return (
            <div className={`${styles.display_flex} ${styles.perm}`}>
              <div className={`${styles.add_mar_right_mid}`}>
                <input type="checkbox" />
              </div>
              <div
                className={`${styles.add_mar_right_mid} ${
                  styles.remoteSchemaPermTypeField
                }`}
              >
                {f}
              </div>
            </div>
          );
        })}
      </div>
    );
  };

  return (
    <div className={`${styles.remoteSchemaPermSelector} ${styles.add_padding}`}>
      {getTypeName()}
      {getFieldsCheckbox()}
    </div>
  );
};

export default GraphQLType;
