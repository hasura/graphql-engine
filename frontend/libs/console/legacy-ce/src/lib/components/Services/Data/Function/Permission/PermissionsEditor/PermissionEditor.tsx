import React from 'react';

import { Button } from '../../../../../../new-components/Button';

import styles from '../../../../../Common/Permissions/PermissionStyles.module.scss';

type PermissionEditorProps = {
  role: string;
  isEditing: boolean;
  closeFn: () => void;
  saveFn: () => void;
  removeFn: () => void;
  permissionAccessInMetadata: 'full' | 'no' | 'partial';
  table: string;
};
const PermissionEditor: React.FC<PermissionEditorProps> = ({
  role,
  isEditing,
  closeFn,
  saveFn,
  removeFn,
  permissionAccessInMetadata,
  table,
}) =>
  isEditing ? (
    <div className={styles.activeEdit}>
      <div className={styles.add_mar_bottom}>
        {permissionAccessInMetadata === 'partial' ? (
          <>
            Partial permissions: please enable <b>select</b> permissions for
            table <b>{table}</b> for role <b>{role}</b> if you want the function
            exposed.
          </>
        ) : (
          <>
            This function is{' '}
            {permissionAccessInMetadata === 'no' ? 'not' : null} allowed for
            role: <b>{role}</b>
          </>
        )}
        <br />
        <br />
        <p>
          Click {permissionAccessInMetadata === 'no' ? '"Save"' : '"Remove"'} if
          you wish to{' '}
          {permissionAccessInMetadata === 'no' ? 'allow' : 'disallow'} it.
        </p>
      </div>
      {permissionAccessInMetadata === 'no' ? (
        <Button onClick={saveFn} mode="primary">
          Save
        </Button>
      ) : (
        <Button onClick={removeFn} mode="destructive">
          Remove
        </Button>
      )}
      <Button onClick={closeFn} mode="default">
        Cancel
      </Button>
    </div>
  ) : null;

export default PermissionEditor;
