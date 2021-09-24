import React from 'react';

import Button from '../../../../../Common/Button';
import { ButtonProps } from '../../../../../Common/Button/Button';

import styles from '../../../../../Common/Permissions/PermissionStyles.scss';

type PermissionsActionButtonProps = {
  onClick: () => void;
  color: ButtonProps['color'];
  text: string;
};
const PermissionsActionButton: React.FC<PermissionsActionButtonProps> = ({
  onClick,
  color,
  text,
}) => (
  <Button color={color} className={styles.add_mar_right} onClick={onClick}>
    {text}
  </Button>
);

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
        <PermissionsActionButton onClick={saveFn} color="yellow" text="Save" />
      ) : (
        <PermissionsActionButton onClick={removeFn} color="red" text="Remove" />
      )}
      <PermissionsActionButton onClick={closeFn} color="white" text="Cancel" />
    </div>
  ) : null;

export default PermissionEditor;
