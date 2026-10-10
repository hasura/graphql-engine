import { GrDocker } from 'react-icons/gr';
import { Flex } from '@radix-ui/themes';
import { Dialog, Text } from '@hasura/shared/ui';
import { DriverInfo } from '@hasura/metadata/data-source';
import { useAgentForm } from '../hooks/useAgentForm';
import { useDockerCommandForm } from '../hooks/useCommandForm';
import { useAddSuperConnectorAgents } from '../hooks/useSuperConnectorAgents';
import { KnownEnterpriseDriver } from '@hasura/shared/types';

export const DockerConfigDialog = ({
  onCancel,
  onSetupSuccess,
  selectedDriver,
}: {
  onCancel: () => void;
  onSetupSuccess: () => void;
  selectedDriver: DriverInfo;
}) => {
  const { AgentForm, watchedValues, agentPath } = useAgentForm();
  const { DockerCommandForm } = useDockerCommandForm(watchedValues);
  const { addAgents, isPending } = useAddSuperConnectorAgents();

  return (
    <Dialog
      title={'Data Connector Agent Setup'}
      footer={{
        callToAction: 'Validate & Connect',
        callToDeny: 'Cancel',
        onClose: () => {
          onCancel();
        },
        onSubmit: async () => {
          const { success, makeToast } = await addAgents(
            agentPath,
            selectedDriver?.name as KnownEnterpriseDriver,
          );

          makeToast();

          if (success) {
            onSetupSuccess();
          }
        },
        isLoading: isPending,
      }}
    >
      <Flex direction="column" gap="2">
        <Flex align="center" gap="2">
          <GrDocker />
          <Text className="ml-1" weight="bold">
            Docker Setup
          </Text>
        </Flex>
        <Text as="div">
          Run the command below to install the Hasura Data Connector Service.
        </Text>
        {DockerCommandForm()}
        {AgentForm()}
      </Flex>
    </Dialog>
  );
};
