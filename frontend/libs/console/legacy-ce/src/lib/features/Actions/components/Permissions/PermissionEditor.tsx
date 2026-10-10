import { Flex } from '@radix-ui/themes';
import { Button, Card, useDestructiveConfirm } from '@hasura/shared/ui';
import useDropActionPermission from '../../hooks/useDropActionPermission';
import useSaveActionPermission from '../../hooks/useSaveActionPermission';
import type { Action } from '@hasura/shared/types';

export type ActionPermissionState =
  'none' | 'new_permission' | 'new_role' | 'modify';

type Props = {
  currentAction: Action;
  state: ActionPermissionState;
  role: string;
  onClose: () => void;
};

const PermissionEditor = ({ state, currentAction, role, onClose }: Props) => {
  const { dropActionPermission, isPending: isDropping } =
    useDropActionPermission();
  const { savePermission, isPending: isSaving } = useSaveActionPermission();
  const destructiveConfirm = useDestructiveConfirm();

  const permText =
    state === 'modify' ? (
      <div>
        This action is allowed for role: <b>{role}</b>
        <br />
        Click &quot;Remove&quot; if you wish to disallow it.
      </div>
    ) : (
      <div>
        Click save to allow this action for role: <b>{role}</b>
      </div>
    );

  const saveFunc = () => {
    savePermission(
      {
        action: currentAction.name,
        role,
      },
      {
        onSuccess: () => {
          onClose();
        },
      },
    );
  };

  const getSaveButton = () => {
    return (
      <Button
        onClick={saveFunc}
        mode="primary"
        loading={isSaving}
        data-test="save-permissions-for-action"
      >
        Save
      </Button>
    );
  };

  const removeFunc = () => {
    destructiveConfirm({
      resourceName: currentAction.name,
      resourceType: 'action',
      onConfirm: () => {
        return new Promise((resolve) => {
          dropActionPermission(
            { role, action: currentAction.name },
            {
              onSuccess: () => {
                onClose();
                resolve(true);
              },
              onError: () => {
                resolve(false);
              },
            },
          );
        });
      },
    });
  };

  const getRemoveButton = () => {
    return (
      <Button
        onClick={removeFunc}
        mode="destructive"
        size="md"
        loading={isDropping}
        disabled={isSaving}
      >
        Remove
      </Button>
    );
  };

  const getCancelButton = () => {
    return (
      <div className="ml-2">
        <Button mode="default" onClick={onClose}>
          Cancel
        </Button>
      </div>
    );
  };

  return (
    <Card>
      <div className="mb-4">{permText}</div>
      <Flex>
        {state === 'modify' ? getRemoveButton() : getSaveButton()}
        {getCancelButton()}
      </Flex>
    </Card>
  );
};

export default PermissionEditor;
