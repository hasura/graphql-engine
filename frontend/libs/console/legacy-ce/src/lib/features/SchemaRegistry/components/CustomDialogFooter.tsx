import React from 'react';
import { Button } from '../../../new-components/Button';
import { Analytics } from '../../Analytics';

type CustomDialogFooterProps = {
  onSet: () => void;
  onClose: () => void;
  isLoading: boolean;
};

export const CustomDialogFooter: React.FC<CustomDialogFooterProps> = ({
  onSet,
  onClose,
  isLoading,
}) => {
  return (
    <div className="flex justify-between border-t border-gray-300 bg-white p-sm">
      <div>
        <p className="text-muted">
          Email alerts will be sent to the owner of this project.
        </p>
      </div>
      <div className="flex">
        <Button onClick={onClose}>Cancel</Button>
        <Analytics name="data-schema-registry-alerts-set-btn">
          <div className="ml-2">
            <Button
              mode="primary"
              onClick={e => {
                e.preventDefault();
                onSet();
              }}
              isLoading={isLoading}
              disabled={isLoading}
            >
              Set
            </Button>
          </div>
        </Analytics>
      </div>
    </div>
  );
};
