import React from 'react';
import styles from '../RemoteSchema.scss';
import GraphQLType from './GraphQLType';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';

const PermissionsEditor = ({ role, types }) => {
  const editorExpanded = () => {
    const roleTextbox = (
      <div className={`${styles.add_mar_bottom}`}>
        <div className={`${styles.add_mar_bottom_small}`}>
          <b>Role:</b>
        </div>
        <div>
          <input type="text" className={`form-control ${styles.wd300Px}`} />
        </div>
      </div>
    );

    const permSelector = (
      <div>
        <div className={`${styles.add_mar_bottom_small}`}>
          <b>Allowed Types:</b>
        </div>
        <GraphQLType
          typeName={'query_root'}
          fields={{
            field1: {
              typeName: 'Type1',
              isScalar: false,
              isChecked: false,
            },
            field2: {
              typeName: 'Type1',
              isScalar: false,
              isChecked: true,
            },
            field3: {
              typeName: 'Type2',
              isScalar: true,
              isChecked: true,
            },
            field4: {
              typeName: 'Type3',
              isScalar: false,
              isChecked: false,
            },
          }}
        />
      </div>
    );

    return (
      <div>
        {roleTextbox}
        {permSelector}
      </div>
    );
  };

  const collapsedLabel = () => <b>{role}</b>;

  console.log(types);

  return (
    <div>
      <ExpandableEditor
        editorExpanded={editorExpanded}
        property={'remote-relationship-add'}
        service="table-relationship"
        saveFunc={() => null}
        expandButtonText={'Edit'}
        collapseButtonText={'Close'}
        collapsedLabel={collapsedLabel}
        removeFunc={() => null}
      />
    </div>
  );
};

export default PermissionsEditor;
