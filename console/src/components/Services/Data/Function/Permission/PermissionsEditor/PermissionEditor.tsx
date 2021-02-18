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
  isPermSet: boolean;
};
const PermissionEditor: React.FC<PermissionEditorProps> = ({
  role,
  isEditing,
  closeFn,
  saveFn,
  removeFn,
  isPermSet,
}) =>
  isEditing ? (
    <div className={styles.activeEdit}>
      <div className={styles.add_mar_bottom}>
        This function is {!isPermSet ? 'not' : null} allowed for role:{' '}
        <b>{role}</b>
        <br />
        Click {!isPermSet ? '"Save"' : '"Remove"'} if you wish to{' '}
        {!isPermSet ? 'allow' : 'disallow'} it.
      </div>
      {!isPermSet ? (
        <PermissionsActionButton onClick={saveFn} color="yellow" text="Save" />
      ) : (
        <PermissionsActionButton onClick={removeFn} color="red" text="Remove" />
      )}
      <PermissionsActionButton onClick={closeFn} color="white" text="Cancel" />
    </div>
  ) : null;

export default PermissionEditor;
