import React from 'react';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { Flex } from '@radix-ui/themes';
import {
  Button,
  Dialog,
  CheckboxField,
  IndicatorCard,
  Text,
} from '@hasura/shared/ui';
import { setLSItem } from '@hasura/shared/utils';

import { LS_KEYS } from '@hasura/shared/types';

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
      enabled ? 'disabled' : 'enabled',
    );
  };

  return (
    <Flex align="center" justify="between" className="p-2">
      <div className="grow">
        <CheckboxField
          name="noPermissionsConfirmationDialog"
          fieldProps={{
            onChange: (enabled) => {
              storeDoNotShowPermissionsDialogFlag(enabled);
            },
          }}
        >
          <div>Don&apos;t ask me again</div>
        </CheckboxField>
      </div>
      <Flex align="center" gap="2">
        <Button mode="default" onClick={onClose}>
          Cancel
        </Button>
        <Button mode="primary" onClick={onSubmit}>
          Disable
        </Button>
      </Flex>
    </Flex>
  );
};

export type Props = {
  onSubmit: () => void;
  onClose: () => void;
  title: React.ReactElement<any>;
  description: React.ReactElement<any>;
};

export const PermissionsConfirmationModal: React.FC<Props> = ({
  onSubmit,
  onClose,
  title,
  description,
}) => {
  return (
    <Dialog
      footer={<CustomDialogFooter onSubmit={onSubmit} onClose={onClose} />}
    >
      <Analytics name="PermissionsConfirmationModal" {...REDACT_EVERYTHING}>
        <Flex className="items-top p-4">
          <IndicatorCard status="warning" showIcon>
            <div>
              <Text as="div" weight="bold">
                {title}
              </Text>
              <div className="overflow-y-auto max-h-[calc(100vh-14rem)]">
                <Text>{description}</Text>
              </div>
            </div>
          </IndicatorCard>
        </Flex>
      </Analytics>
    </Dialog>
  );
};
