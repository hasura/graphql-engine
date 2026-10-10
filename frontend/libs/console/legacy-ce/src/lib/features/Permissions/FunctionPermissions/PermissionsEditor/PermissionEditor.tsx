import React from 'react';
import { Button } from '@hasura/shared/ui';
import { AccessType } from '@hasura/shared/types';

type PermissionEditorProps = {
  role: string;
  isEditing: boolean;
  closeFn: () => void;
  saveFn: () => void;
  removeFn: () => void;
  permissionAccessInMetadata: AccessType;
  table: string;
  isSaving: boolean;
  isDeleting: boolean;
};
const PermissionEditor: React.FC<PermissionEditorProps> = ({
  role,
  isEditing,
  closeFn,
  saveFn,
  removeFn,
  permissionAccessInMetadata,
  table,
  isSaving,
  isDeleting,
}) =>
  isEditing ? (
    <div>
      <div className="mb-6">
        {permissionAccessInMetadata === 'partialAccess' ||
        permissionAccessInMetadata === 'partialAccessWarning' ? (
          <>
            Partial permissions: please enable <b>select</b> permissions for
            table <b>{table}</b> for role <b>{role}</b> if you want the function
            exposed.
          </>
        ) : (
          <>
            This function is{' '}
            {permissionAccessInMetadata === 'noAccess' ? 'not' : null} allowed
            for role: <b>{role}</b>
          </>
        )}
        <br />
        <br />
        <p>
          Click{' '}
          {permissionAccessInMetadata === 'noAccess' ? '"Save"' : '"Remove"'} if
          you wish to{' '}
          {permissionAccessInMetadata === 'noAccess' ? 'allow' : 'disallow'} it.
        </p>
      </div>
      {permissionAccessInMetadata === 'noAccess' ? (
        <Button
          onClick={saveFn}
          mode="primary"
          loading={isSaving}
          disabled={isSaving || isDeleting}
        >
          Save
        </Button>
      ) : (
        <Button
          onClick={removeFn}
          mode="destructive"
          loading={isDeleting}
          disabled={isSaving || isDeleting}
        >
          Remove
        </Button>
      )}
      <Button
        onClick={closeFn}
        mode="default"
        disabled={isSaving || isDeleting}
      >
        Cancel
      </Button>
    </div>
  ) : null;

export default PermissionEditor;
