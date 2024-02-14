import { GrDocker } from 'react-icons/gr';
import { Dialog } from '../../../../../new-components/Dialog';
import { DriverInfo } from '../../../../DataSource';
import { useAgentForm } from '../hooks/useAgentForm';
import { useDockerCommandForm } from '../hooks/useCommandForm';
import { useAddSuperConnectorAgents } from '../hooks/useSuperConnectorAgents';

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

  const { addAgents, isLoading } = useAddSuperConnectorAgents();

  return (
    <Dialog
      hasBackdrop
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
            selectedDriver?.name
          );

          makeToast();

          if (success) {
            onSetupSuccess();
          }
        },
        isLoading,
      }}
    >
      <div className="p-4">
        <div className="flex flex-col">
          <div className="font-bold text-muted flex items-center">
            <GrDocker />
            <div className="ml-1">Docker Setup</div>
          </div>
          <div>
            Run the command below to install the Hasura Data Connector Service.
          </div>
          <input
            type="text"
            className="h-0 w-0 p-0 border-none outline-none"
            style={{ boxShadow: 'none' }}
          />
          {DockerCommandForm()}
          {AgentForm()}
        </div>
      </div>
    </Dialog>
  );
};
