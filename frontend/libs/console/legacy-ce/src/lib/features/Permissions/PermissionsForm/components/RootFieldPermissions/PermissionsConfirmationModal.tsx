import React from 'react';
import { Analytics, REDACT_EVERYTHING } from '../../../../Analytics';
import { FaExclamationTriangle } from 'react-icons/fa';
import { Button } from '../../../../../new-components/Button';
import { Dialog } from '../../../../../new-components/Dialog';
import { LS_KEYS, setLSItem } from '../../../../../utils/localStorage';
import { Checkbox } from '../../../../../new-components/Form';

type CustomDialogFooterProps = {
  onSubmit: () => void;
  onClose: () => void;
};

const CustomDialogFooter: React.FC<CustomDialogFooterProps> = ({
  onClose,
  onSubmit,
}) => {
  const storeDoNotShowPermissionsDialogFlag = (enabled: string | boolean) => {
    setLSItem(
      LS_KEYS.permissionConfirmationModalStatus,
      enabled ? 'disabled' : 'enabled'
    );
  };

  return (
    <div className="flex items-center border-t border-gray-300 bg-white p-sm">
      <div className="flex-grow">
        <Checkbox
          name="noPermissionsConfirmationDialog"
          onCheckedChange={enabled => {
            storeDoNotShowPermissionsDialogFlag(enabled);
          }}
        >
          <div>Don&apos;t ask me again</div>
        </Checkbox>
      </div>
      <div className="flex">
        <Button onClick={onClose}>Cancel</Button>
        <div className="ml-2">
          <Button mode="primary" onClick={onSubmit}>
            Disable
          </Button>
        </div>
      </div>
    </div>
  );
};

export type Props = {
  onSubmit: () => void;
  onClose: () => void;
  title: React.ReactElement;
  description: React.ReactElement;
};

export const PermissionsConfirmationModal: React.FC<Props> = ({
  onSubmit,
  onClose,
  title,
  description,
}) => {
  return (
    <Dialog
      title=""
      hasBackdrop
      footer={<CustomDialogFooter onSubmit={onSubmit} onClose={onClose} />}
    >
      <Analytics name="PermissionsConfirmationModal" {...REDACT_EVERYTHING}>
        <div className="flex items-top p-md">
          <div className="text-yellow-500">
            <FaExclamationTriangle className="w-9 h-9 mr-md fill-current" />
          </div>
          <div>
            <p className="font-semibold">{title}</p>
            <div className="overflow-y-auto max-h-[calc(100vh-14rem)]">
              <p className="m-0">{description}</p>
            </div>
          </div>
        </div>
      </Analytics>
    </Dialog>
  );
};
