import React from 'react';
import { Button, hasuraToast, Text, useHasuraAlert } from '@hasura/shared/ui';
import { useResetMetadata } from '@hasura/metadata/api';
import { Strong } from '@radix-ui/themes';

const CONFIRMATION_TEXT = 'CONFIRM';

const ResetMetadata = () => {
  const { mutateAsync: resetMetadata, isPending: isResetting } =
    useResetMetadata();
  const { hasuraPrompt } = useHasuraAlert();

  const handleReset = (e: React.MouseEvent<HTMLButtonElement>) => {
    e.preventDefault();
    hasuraPrompt({
      title: 'Reset metadata',
      message:
        'This will permanently reset the Hasura metadata related to your tables, remote schemas, actions and triggers. This action cannot be undone.',
      destructive: true,
      confirmText: 'Reset',
      promptLabel: (
        <div className="py-2">
          <Text>
            Type <Strong>{CONFIRMATION_TEXT}</Strong> to confirm this action.
          </Text>
        </div>
      ),
      onCloseAsync: async (result) => {
        if (!result.confirmed) {
          return { withSuccess: false };
        }

        if (result.promptValue !== CONFIRMATION_TEXT) {
          hasuraToast({
            type: 'error',
            title: 'Entry Not Matching',
            message: `Your entry "${result.promptValue}" does not match "${CONFIRMATION_TEXT}".`,
          });
          return { withSuccess: false };
        }

        // useResetMetadata handles its own success / error toasts and never
        // rejects, so we close the dialog silently and let those toasts report
        // the outcome.
        return resetMetadata()
          .then(() => ({
            withSuccess: true,
          }))
          .catch(() => ({
            withSuccess: false,
          }));
      },
    });
  };

  return (
    <div className="inline-block">
      <Button
        data-test="data-reset-metadata"
        loading={isResetting}
        loadingText="Resetting..."
        mode="default"
        onClick={handleReset}
        size="2"
      >
        Reset
      </Button>
    </div>
  );
};

export default ResetMetadata;
