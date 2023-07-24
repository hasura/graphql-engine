import React from 'react';
import { Button } from './../../../new-components/Button';
import { AlertHeader } from './AlertsHeader';
import { FaBell, FaTimes } from 'react-icons/fa';
import Tooltip from '../../../components/Common/Tooltip/Tooltip';
import {
  useSlackIntegration,
  SlackOauthStatus,
} from '../hooks/useSlackIntegration';
import globals from '../../../Globals';
import { useGetSlackState } from '../hooks/useGetSlackState';
import { SlackButtonSVG } from './SlackButtonSvg';
import { FaExclamationTriangle, FaCheck, FaTrash } from 'react-icons/fa';
import { SlackDeleteConfirmationDialog } from './SlackDeleteConfirmationDialog';
import { Analytics } from '../../Analytics';

type TileProp = {
  statusIcon: React.ReactNode;
  actionName: string;
  actionIcon: React.ReactNode;
  message: React.ReactNode;
};

const getSlackIntegrationStatusTileProps = (
  slackStatus: SlackOauthStatus,
  onAction: () => void
): TileProp | null => {
  switch (slackStatus.status) {
    case 'error': {
      return {
        statusIcon: (
          <FaExclamationTriangle className="fill-current h-4 w-4 mr-8 shrink-0 text-gray-500" />
        ),
        actionName: 'dismiss-slack-integration-error',
        actionIcon: (
          <Tooltip message="Dismiss">
            <FaTimes
              className="fill-current cursor-pointer text-muted hover:text-gray-800 ml-2"
              onClick={onAction}
            />
          </Tooltip>
        ),
        message: <p>{slackStatus.error.message}</p>,
      };
    }
    case 'authenticated': {
      return {
        statusIcon: (
          <FaCheck className="fill-current mt-[3px] h-4 w-4 mr-8 shrink-0 text-emerald-500" />
        ),
        actionName: 'delete-slack-integration',
        actionIcon: (
          <Tooltip message="Remove Slack Integration">
            <FaTrash
              className="fill-current cursor-pointer text-muted hover:text-gray-800 ml-2"
              onClick={onAction}
            />
          </Tooltip>
        ),
        message: (
          <div>
            Slack Integration for alerts on this project has been configured for
            the channel <b>{slackStatus.channelName}</b> for{' '}
            <b>{slackStatus.teamName}</b> workspace.
          </div>
        ),
      };
    }
    default: {
      return null;
    }
  }
};

type SlackIntegrationStatusTileProps = {
  status: SlackOauthStatus;
  onAction: () => void;
};

export const SlackIntegrationStatusTile: React.FC<
  SlackIntegrationStatusTileProps
> = props => {
  const { status, onAction } = props;

  const slackIntegrationTileProps = getSlackIntegrationStatusTileProps(
    status,
    onAction
  );

  if (!slackIntegrationTileProps) return null;

  const { statusIcon, actionName, actionIcon, message } =
    slackIntegrationTileProps;
  return (
    <div className="flex justify-between border rounded mx-6 p-2 mb-4 text-gray-500">
      <div className="flex justify-start items-center">
        {statusIcon}
        {message}
      </div>
      <Analytics name={`settings-schema-registry-slack-alerts-${actionName}`}>
        <div className="flex">{actionIcon}</div>
      </Analytics>
    </div>
  );
};

type SlackIntegrationStateProps = {
  slackIntegrationStatus: SlackOauthStatus;
  onIntegrationSlack: () => void;
  onDelete: () => void;
  onDismiss: () => void;
};

export const SlackIntegrationState = (props: SlackIntegrationStateProps) => {
  const { slackIntegrationStatus, onIntegrationSlack, onDelete, onDismiss } =
    props;
  const isButtonDisabled = slackIntegrationStatus.status === 'authenticating';

  switch (slackIntegrationStatus.status) {
    case 'idle':
    case 'authenticating':
      return (
        <div className="flex justify-center mb-4">
          <Analytics name="settings-schema-registry-add-to-slack-btn">
            <Button
              onClick={onIntegrationSlack}
              mode="default"
              data-testid="onboarding-wizard-neon-connect-db-button"
              isLoading={isButtonDisabled}
              disabled={isButtonDisabled}
            >
              <div className="flex justify-center">
                <SlackButtonSVG />
                <p className="ml-2">Add to Slack</p>
              </div>
            </Button>
          </Analytics>
        </div>
      );
    case 'authenticated': {
      return (
        <SlackIntegrationStatusTile
          status={slackIntegrationStatus}
          onAction={onDelete}
        />
      );
    }
    case 'error':
      return (
        <SlackIntegrationStatusTile
          status={slackIntegrationStatus}
          onAction={onDismiss}
        />
      );
  }
};

type SlackAlertsProps = {
  onClose: () => void;
};

export const SlackAlerts: React.FC<SlackAlertsProps> = ({ onClose }) => {
  const projectID = globals.hasuraCloudProjectId || '';

  const slackState = useGetSlackState(projectID);

  const doesSlackIntegrationExist =
    slackState.kind === 'success' && slackState.response.slack_config.length;

  const slackChannel = doesSlackIntegrationExist
    ? slackState.response.slack_config[0].channel_name
    : '';
  const slackTeam = doesSlackIntegrationExist
    ? slackState.response.slack_config[0].team_name
    : '';

  const [
    isSlackDeleteConfirmationModalOpen,
    setIsSlackDeleteConfirmationModalOpen,
  ] = React.useState(false);

  const onCloseDeleteConfirmation = () => {
    setIsSlackDeleteConfirmationModalOpen(false);
  };

  const { slackOauthStatus, startSlackOAuth, deleteSlackApp } =
    useSlackIntegration(onCloseDeleteConfirmation);

  const [slackIntegrationStatus, setSlackIntegrationStatus] =
    React.useState<SlackOauthStatus>(slackOauthStatus);

  const onIntegrateSlack = () => {
    startSlackOAuth();
  };

  const onDelete = () => {
    setIsSlackDeleteConfirmationModalOpen(true);
  };

  const onDeleteSlack = React.useCallback(() => {
    deleteSlackApp.mutate({ projectID: projectID });
  }, [projectID, deleteSlackApp]);

  const onDismiss = React.useCallback(() => {
    if (slackIntegrationStatus.status === 'error') {
      setSlackIntegrationStatus({ status: 'idle' });
    }
  }, [slackIntegrationStatus]);

  React.useEffect(() => {
    setSlackIntegrationStatus(slackOauthStatus);
    if (
      !doesSlackIntegrationExist &&
      slackOauthStatus.status === 'authenticated'
    )
      setSlackIntegrationStatus({ status: 'idle' });
  }, [slackOauthStatus, doesSlackIntegrationExist]);

  return (
    <div className="ml-[-14px]">
      <AlertHeader
        icon={<FaBell className="w-9 h-9 mr-md mt-xs fill-current" />}
        title="Slack Alerts"
        description="Integrate with a Slack channel to enable alerts."
      />

      {doesSlackIntegrationExist ? (
        <SlackIntegrationStatusTile
          status={{
            status: 'authenticated',
            channelName: slackChannel,
            teamName: slackTeam,
          }}
          onAction={onDelete}
        />
      ) : (
        <SlackIntegrationState
          slackIntegrationStatus={slackIntegrationStatus}
          onIntegrationSlack={onIntegrateSlack}
          onDelete={onDelete}
          onDismiss={onDismiss}
        />
      )}
      {isSlackDeleteConfirmationModalOpen && (
        <SlackDeleteConfirmationDialog
          onClose={() => setIsSlackDeleteConfirmationModalOpen(false)}
          onSubmit={onDeleteSlack}
        />
      )}
    </div>
  );
};
