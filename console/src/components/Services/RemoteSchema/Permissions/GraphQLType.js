import React, { useState } from 'react';
import styles from '../RemoteSchema.scss';

const GraphQLType = ({
  typeName,
  fields,
  fieldToggleCallback,
  typeRemovalCallback,
  isRootType,
  isTypeExpanded,
}) => {
  const [isExpanded, setExpansion] = useState(isTypeExpanded);
  const expandType = () => setExpansion(true);
  const collapseType = () => setExpansion(false);

  const removeTypeButton = !isRootType && (
    <div>
      <i
        className={`${styles.fontAwosomeClose} fa-lg fa fa-times`}
        data-test={`remove-type-${typeName}`}
        onClick={typeRemovalCallback}
      />
    </div>
  );

  const getTypeName = () => {
    const onClick = isExpanded ? collapseType : expandType;
    return (
      <div
        className={`${styles.display_flex} ${styles.add_mar_bottom_mid} ${
          styles.cursorPointer
        }`}
        onClick={onClick}
      >
        <i
          className={`fa fa-chevron-${isExpanded ? 'down' : 'right'} ${
            styles.add_mar_right_mid
          }`}
        />
        <b className={`${styles.add_mar_right_mid}`}>{typeName}</b>
        {removeTypeButton}
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
          const toggle = e => {
            fieldToggleCallback(f, e.target.checked);
          };

          return (
            <div
              className={`${styles.display_flex} ${styles.perm} ${
                styles.cursorPointer
              }`}
              key={`${typeName}-${f}`}
              onClick={toggle}
            >
              <div className={`${styles.add_mar_right_mid}`}>
                <input type="checkbox" checked={fields[f].isChecked} readOnly />
              </div>
              <div
                className={`${styles.add_mar_right_mid} ${
                  styles.remoteSchemaPermTypeField
                } ${styles.display_flex}`}
              >
                <div>{f}:&emsp;</div>
                <div>
                  <i>{fields[f].typeName}</i>
                </div>
              </div>
            </div>
          );
        })}
      </div>
    );
  };

  return (
    <div>
      {getTypeName()}
      {getFieldsCheckbox()}
    </div>
  );
};

export default GraphQLType;
