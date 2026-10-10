import React, { useState } from 'react';
import { Box, Flex, Text } from '@radix-ui/themes';
import { Dialog, RadioGroup } from '@hasura/shared/ui';
import {
  useMetadata,
  useUpdateIntrospectionOptions,
} from '@hasura/metadata/api';

type Props = {
  role: string;
  introspectionIsDisabled: boolean;
  onClose: () => void;
};

const IntrospectionForm: React.FC<Props> = ({
  role,
  introspectionIsDisabled,
  onClose,
}) => {
  const { data: meta, refetch: refetchMetadata } = useMetadata();
  const updateIntrospectionOptions = useUpdateIntrospectionOptions();
  // The dialog is mounted per edit, so the initial value only needs reading once.
  const [isDisabled, setIsDisabled] = useState(introspectionIsDisabled);

  const submit = (e?: React.SyntheticEvent) => {
    e?.preventDefault();
    const existingOptions =
      meta?.metadata?.graphql_schema_introspection?.disabled_for_roles ?? [];
    updateIntrospectionOptions(
      {
        existingOptions,
        roleName: role,
        introspectionIsDisabled: isDisabled,
      },
      () => {
        refetchMetadata();
        onClose();
      },
    );
  };

  return (
    <Dialog
      size="md"
      title={`Role: ${role}`}
      onClose={onClose}
      onOpenChange={(open) => {
        if (!open) onClose();
      }}
      footer={{
        callToAction: 'Save Settings',
        callToDeny: 'Cancel',
        onClose,
        onSubmit: submit,
        callToActionProps: {
          disabled: isDisabled === introspectionIsDisabled,
        },
      }}
    >
      <form onSubmit={submit}>
        <Flex gap="6" justify="between" align="start" wrap="wrap">
          <Box className="flex-1 min-w-60">
            <Text as="div" size="2" weight="bold">
              Introspection
            </Text>
            <Text as="div" size="2" color="gray">
              Enable GraphQL schema introspection requests.
            </Text>
          </Box>
          <RadioGroup
            orientation="horizontal"
            value={isDisabled ? 'disabled' : 'enabled'}
            options={[
              { value: 'enabled', label: 'Enabled' },
              { value: 'disabled', label: 'Disabled' },
            ]}
            onChange={(value) => setIsDisabled(value === 'disabled')}
          />
        </Flex>
      </form>
    </Dialog>
  );
};

export default IntrospectionForm;
