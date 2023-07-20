import React from 'react';
import { Dialog } from '../../../new-components/Dialog';

type SlackDeleteConfirmationDialogProps = {
  onClose: () => void;
  onSubmit: () => void;
};

export const SlackDeleteConfirmationDialog: React.FC<
  SlackDeleteConfirmationDialogProps
> = ({ onClose, onSubmit }) => {
  return (
    <Dialog hasBackdrop>
      <>
        <p className="font-bold text-lg ml-4 my-4">
          Are you sure you want to disable Slack alerts?
        </p>
        <Dialog.Footer
          callToDeny="Cancel"
          callToAction="Yes"
          onClose={onClose}
          onSubmit={onSubmit}
          onSubmitAnalyticsName="delete-schema-registry-slack-alerts-submit"
          onCancelAnalyticsName="delete-schema-registry-slack-alerts-cancel"
        />
      </>
    </Dialog>
  );
};
