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
  // state variable of type collapsed or expanded
  const [isExpanded, setExpansion] = useState(isTypeExpanded);
  const expandType = () => setExpansion(true);
  const collapseType = () => setExpansion(false);

  // remove button
  const removeTypeButton = !isRootType && (
    <div>
      <i
        className={`${styles.fontAwosomeClose} fa-lg fa fa-times ${
          styles.cursorPointer
        }`}
        data-test={`remove-type-${typeName}`}
        onClick={typeRemovalCallback}
      />
    </div>
  );

  // typename along with chevron right/down
  const getTypeName = () => {
    const onClick = isExpanded ? collapseType : expandType;
    return (
      <div
        className={`${styles.display_flex} ${styles.add_mar_bottom_mid}`}
        onClick={onClick}
      >
        <div className={`${styles.cursorPointer}`}>
          <i
            className={`fa fa-chevron-${isExpanded ? 'down' : 'right'} ${
              styles.add_mar_right_mid
            }`}
          />
          <b className={`${styles.add_mar_right_mid}`}>{typeName}</b>
        </div>
        {removeTypeButton}
      </div>
    );
  };

  // checkboxes for fields of a type
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
