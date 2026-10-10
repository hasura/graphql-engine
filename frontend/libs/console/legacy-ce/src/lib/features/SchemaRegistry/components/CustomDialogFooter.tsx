import React from 'react';
import { Flex } from '@radix-ui/themes';
import { Button } from '@hasura/shared/ui';
import { Analytics } from '@hasura/shared/analytics';

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
    <Flex justify="between" className="border-t border-gray-300 bg-white p-2">
      <div>
        <p className="text-muted">
          Email alerts will be sent to the owner of this project.
        </p>
      </div>
      <Flex>
        <Button onClick={onClose}>Cancel</Button>
        <Analytics name="data-schema-registry-alerts-set-btn">
          <div className="ml-2">
            <Button
              mode="primary"
              onClick={(e) => {
                e.preventDefault();
                onSet();
              }}
              loading={isLoading}
              disabled={isLoading}
            >
              Set
            </Button>
          </div>
        </Analytics>
      </Flex>
    </Flex>
  );
};
