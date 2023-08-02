import React, { useState, Dispatch, ChangeEvent } from 'react';
import { FaEye, FaTimes } from 'react-icons/fa';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';
import { Button } from '../../../../new-components/Button';
import { Tooltip } from '../../../../new-components/Tooltip';
import { Collapse } from '../../../../new-components/deprecated';
import {
  ReadReplicaState,
  ReadReplicaActions,
  ConnectDBState,
  ConnectDBActions,
  ExtendedConnectDBState,
  connectionTypes,
} from './state';
import ConnectDatabaseForm from './ConnectDBForm';
import {
  makeConnectionStringFromConnectionParams,
  parseURI,
} from './ManageDBUtils';
import { useEELiteAccess } from '../../../../features/EETrial';

import styles from './DataSources.module.scss';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';
import { EETrialCard } from '../../../../features/EETrial/components/EETrialCard/EETrialCard';
import globals from '../../../../Globals';
import { isProConsole } from '../../../../utils';

const checkIfFieldsAreEmpty = (
  currentReadReplicaConnectionType: string,
  currentReadReplicaState: ConnectDBState
) => {
  // split into 3 different conditions for better readability
  if (
    currentReadReplicaConnectionType === connectionTypes.DATABASE_URL &&
    !currentReadReplicaState?.databaseURLState?.dbURL
  ) {
    return true;
  }
  if (
    currentReadReplicaConnectionType === connectionTypes.ENV_VAR &&
    !currentReadReplicaState?.envVarState?.envVar
  ) {
    return true;
  }
  if (
    currentReadReplicaConnectionType === connectionTypes.CONNECTION_PARAMS &&
    !currentReadReplicaState?.connectionParamState?.database &&
    !currentReadReplicaState?.connectionParamState?.host &&
    !currentReadReplicaState?.connectionParamState?.port &&
    !currentReadReplicaState?.connectionParamState?.username
  ) {
    return true;
  }
  return false;
};

type ReadReplicaProps = {
  readReplicaState: ReadReplicaState;
  readReplicaDispatch: Dispatch<ReadReplicaActions>;
  connectDBState: ConnectDBState;
  connectDBStateDispatch: Dispatch<ConnectDBActions>;
  readReplicaConnectionType: string;
  updateReadReplicaConnectionType: (e: ChangeEvent<HTMLInputElement>) => void;
  onClickAddReadReplicaCb?: () => void;
  onClickCancelOnReadReplicaCb?: () => void;
  onClickSaveReadReplicaCb?: () => void;
};

interface FormProps
  extends Pick<
    ReadReplicaProps,
    | 'connectDBState'
    | 'connectDBStateDispatch'
    | 'readReplicaConnectionType'
    | 'updateReadReplicaConnectionType'
  > {
  onClickCancel: () => void;
  onClickSave: () => void;
}

const Form: React.FC<FormProps> = ({
  connectDBState,
  connectDBStateDispatch,
  onClickCancel,
  onClickSave,
  readReplicaConnectionType,
  updateReadReplicaConnectionType,
}) => {
  const areFieldsEmpty = checkIfFieldsAreEmpty(
    readReplicaConnectionType,
    connectDBState
  );

  return (
    <div className={styles.read_replica_add_form}>
      <ConnectDatabaseForm
        connectionDBState={connectDBState}
        connectionDBStateDispatch={connectDBStateDispatch}
        updateConnectionTypeRadio={updateReadReplicaConnectionType}
        connectionTypeState={readReplicaConnectionType}
        isReadReplica
        title="Connect Read Replica via"
      />
      <div className={styles.read_replica_action_bar}>
        <Button size="md" mode="default" onClick={onClickCancel}>
          Cancel
        </Button>
        <Button
          mode="primary"
          size="md"
          onClick={onClickSave}
          disabled={areFieldsEmpty}
        >
          Add Read Replica
        </Button>
      </div>
    </div>
  );
};

type ReadReplicaListItemProps = {
  currentState: ExtendedConnectDBState;
  onClickRemove: () => void;
};

const ReadReplicaListItem: React.FC<ReadReplicaListItemProps> = ({
  currentState,
  onClickRemove,
}) => {
  const [showUrl, setShowUrl] = useState(false);
  const connectionType = currentState.chosenConnectionType;
  const isFromEnvVar = connectionType === connectionTypes.ENV_VAR;
  let connectionString = '';
  if (!isFromEnvVar) {
    if (connectionType === connectionTypes.DATABASE_URL) {
      connectionString = currentState?.databaseURLState?.dbURL?.trim() ?? '';
    } else {
      connectionString = makeConnectionStringFromConnectionParams({
        dbType: 'postgres',
        ...currentState.connectionParamState,
      });
    }
  }
  const host = isFromEnvVar ? '' : parseURI(connectionString)?.host ?? '';
  return (
    <div
      className={styles.read_replica_list_item}
      key={`read-replica-item-${currentState.displayName}`}
    >
      <span>
        <Button
          mode="destructive"
          size="sm"
          onClick={onClickRemove}
          className={styles.remove_replica_btn}
        >
          Remove
        </Button>
      </span>

      <p>{isFromEnvVar ? currentState.envVarState.envVar : host}</p>
      {/* The connection string is redundant if it's provided via ENV VAR */}
      {!isFromEnvVar && (
        <span className="px-sm py-xs max-w-xs align-top break-all">
          {showUrl ? (
            connectionString
          ) : (
            <span
              className="text-secondary flex items-center cursor-pointer"
              onClick={() => setShowUrl(true)}
            >
              <FaEye aria-hidden="true" />
              <span style={{ marginLeft: 6 }}>Show Connection String</span>
            </span>
          )}
          {showUrl && (
            <Tooltip
              side="right"
              tooltipContentChildren="Hide connection string"
            >
              <FaTimes
                className={`${styles.closeHeader}`}
                aria-hidden="true"
                onClick={() => setShowUrl(false)}
              />
            </Tooltip>
          )}
        </span>
      )}
    </div>
  );
};

const ReadReplicaForm: React.FC<ReadReplicaProps> = ({
  connectDBState,
  connectDBStateDispatch,
  readReplicaState,
  readReplicaDispatch,
  onClickAddReadReplicaCb,
  onClickCancelOnReadReplicaCb,
  onClickSaveReadReplicaCb,
  readReplicaConnectionType,
  updateReadReplicaConnectionType,
}) => {
  const { access: eeLiteAccess } = useEELiteAccess(globals);
  const [isReadReplicaButtonClicked, updateClickState] = useState(false);

  const onClickAddReadReplica = () => {
    updateClickState(true);
    if (onClickAddReadReplicaCb) {
      onClickAddReadReplicaCb();
    }
  };

  const onClickCancelOnReadReplica = () => {
    updateClickState(false);
    if (onClickCancelOnReadReplicaCb) {
      onClickCancelOnReadReplicaCb();
    }
  };

  const onClickSaveReadReplica = () => {
    updateClickState(false);
    if (onClickSaveReadReplicaCb) {
      onClickSaveReadReplicaCb();
    }
  };

  const onClickRemoveReadReplica = (replicaDBName: string) => () =>
    readReplicaDispatch({
      type: 'REMOVE_READ_REPLICA',
      data: replicaDBName,
    });

  return (
    <>
      {
        // eslint-disable-next-line no-underscore-dangle
        eeLiteAccess !== 'forbidden' || isProConsole(globals) ? (
          <Analytics name="EditDataSource" {...REDACT_EVERYTHING}>
            <Collapse
              title="Read Replicas"
              defaultOpen={eeLiteAccess !== 'active'}
            >
              <Collapse.Content>
                <div className={`${styles.flexColumn} my-1.5`}>
                  <p>
                    Hasura can load balance queries and subscriptions across
                    read replicas while sending all mutations and metadata API
                    calls to the master.
                    <LearnMoreLink href="https://hasura.io/docs/latest/graphql/cloud/read-replicas.html" />
                  </p>
                  {readReplicaState.map((stateVar, index) => (
                    <ReadReplicaListItem
                      currentState={stateVar}
                      onClickRemove={onClickRemoveReadReplica(
                        stateVar.displayName
                      )}
                      key={index}
                    />
                  ))}
                  {eeLiteAccess === 'eligible' ||
                  eeLiteAccess === 'expired' ||
                  eeLiteAccess === 'deactivated' ? (
                    <EETrialCard
                      cardTitle="Improve performance and handle increased traffic with read replicas"
                      id="read-replicas-legacy"
                      cardText={
                        <span>
                          Scale your database by offloading read queries to
                          read-only replicas, allowing for better performance
                          and availability for users.
                        </span>
                      }
                      buttonLabel="Enable Enterprise"
                      buttonType="default"
                      eeAccess={eeLiteAccess}
                      horizontal
                    />
                  ) : null}
                  {(isProConsole(globals) || eeLiteAccess === 'active') &&
                    (!isReadReplicaButtonClicked ? (
                      <span className="py-1.5">
                        <Button
                          size="sm"
                          onClick={onClickAddReadReplica}
                          className={styles.add_button_styles}
                        >
                          Add Read Replica
                        </Button>
                      </span>
                    ) : (
                      <Form
                        connectDBState={connectDBState}
                        connectDBStateDispatch={connectDBStateDispatch}
                        onClickCancel={onClickCancelOnReadReplica}
                        onClickSave={onClickSaveReadReplica}
                        readReplicaConnectionType={readReplicaConnectionType}
                        updateReadReplicaConnectionType={
                          updateReadReplicaConnectionType
                        }
                      />
                    ))}
                </div>
              </Collapse.Content>
            </Collapse>
          </Analytics>
        ) : null
      }
      <Analytics name="EditDataSource" children={null} {...REDACT_EVERYTHING} />
    </>
  );
};

export default ReadReplicaForm;
