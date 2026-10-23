import { useCallback, useRef, useState } from 'react';
import { Checkbox, hasuraToast, useHasuraAlert } from '@hasura/shared/ui';
import { useRemoveRemoteSchema } from '@hasura/metadata/api';

// Message body for the delete-remote-schema dialog. Exposes the server's
// `cascade` option as an explicit, unchecked-by-default opt-in and warns that
// enabling it permanently drops dependent objects (e.g. remote relationships
// defined against this remote schema). The selected value is reported back via
// `onCascadeChange` so the confirmation handler can forward it to the API.
export const RemoteSchemaDeleteWarning = ({
  remoteSchemaName,
  onCascadeChange,
}: {
  remoteSchemaName: string;
  onCascadeChange: (cascade: boolean) => void;
}) => {
  const [cascade, setCascade] = useState(false);

  return (
    <div className="py-2">
      <p>
        This will remove the remote GraphQL schema{' '}
        <strong>{remoteSchemaName}</strong> from your GraphQL schema.
      </p>
      <div className="mt-3">
        <Checkbox
          align="start"
          value={cascade}
          onChange={(checked) => {
            const next = checked === true;
            setCascade(next);
            onCascadeChange(next);
          }}
        >
          <span>
            Also drop all dependent objects (cascade). Enable this only if the
            remote schema has dependent objects such as remote relationships —
            they will be permanently removed too. When left unchecked, the
            server refuses the deletion and lists any dependent objects instead.
          </span>
        </Checkbox>
      </div>
    </div>
  );
};

/**
 * Encapsulates the "delete remote schema" confirmation dialog. It requires the
 * remote schema name to be typed to confirm, exposes `cascade` as an explicit
 * unchecked-by-default opt-in, and forwards the selected value to
 * `remove_remote_schema`. `cascadeRef` is reset to `false` every time the
 * dialog is opened so re-opening always defaults back to "no cascade".
 */
export const useRemoteSchemaDeletePrompt = () => {
  const { hasuraPrompt } = useHasuraAlert();
  const { mutate: remove, isPending: isRemoving } = useRemoveRemoteSchema();
  const cascadeRef = useRef(false);

  const promptDelete = useCallback(
    (remoteSchemaName: string, onDeleted?: () => void) => {
      cascadeRef.current = false;

      hasuraPrompt({
        title: 'Delete remote schema',
        message: (
          <RemoteSchemaDeleteWarning
            remoteSchemaName={remoteSchemaName}
            onCascadeChange={(cascade) => {
              cascadeRef.current = cascade;
            }}
          />
        ),
        destructive: true,
        confirmText: 'Delete',
        promptLabel: (
          <div className="py-2">
            Type <strong>{remoteSchemaName}</strong> to confirm this action.
          </div>
        ),
        onCloseAsync: async (result) => {
          if (!result.confirmed) {
            return { withSuccess: false };
          }

          if (result.promptValue !== remoteSchemaName) {
            hasuraToast({
              type: 'error',
              title: 'Entry Not Matching',
              message: `Your entry "${result.promptValue}" does not match "${remoteSchemaName}".`,
            });
            return { withSuccess: false };
          }

          return new Promise<{ withSuccess: boolean; successText?: string }>(
            (resolve) => {
              remove(
                { name: remoteSchemaName, cascade: cascadeRef.current },
                () => {
                  onDeleted?.();
                  resolve({
                    withSuccess: true,
                    successText: `Deleted ${remoteSchemaName}`,
                  });
                },
                () => {
                  resolve({ withSuccess: false });
                },
              );
            },
          );
        },
      });
    },
    [hasuraPrompt, remove],
  );

  return { promptDelete, isRemoving };
};
