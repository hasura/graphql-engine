import React, { useState, Dispatch, ChangeEvent } from 'react';

import {
  ReadReplicaState,
  ReadReplicaActions,
  ConnectDBState,
  ConnectDBActions,
  ExtendedConnectDBState,
  connectionTypes,
} from './state';
import ConnectDatabaseForm from './ConnectDBForm';
import Button from '../../../Common/Button';
import ToolTip from '../../../Common/Tooltip/Tooltip';
import {
  makeConnectionStringFromConnectionParams,
  parseURI,
} from './ManageDBUtils';

import styles from './DataSources.scss';

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
        isreadreplica
        title="Connect Read Replica via"
      />
      <div className={styles.read_replica_action_bar}>
        <Button size="sm" color="white" onClick={onClickCancel}>
          Cancel
        </Button>
        <Button
          size="sm"
          color="yellow"
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
      <Button
        color="white"
        size="xs"
        onClick={onClickRemove}
        className={styles.remove_replica_btn}
      >
        Remove
      </Button>
      <p>{isFromEnvVar ? currentState.envVarState.envVar : host}</p>
      {/* The connection string is redundant if it's provided via ENV VAR */}
      {!isFromEnvVar && (
        <span
          className={`${styles.db_large_string_break_words} ${styles.add_pad_top_10}`}
        >
          {showUrl ? (
            connectionString
          ) : (
            <span
              className={styles.show_connection_string}
              onClick={() => setShowUrl(true)}
            >
              <i
                className={`${styles.showAdminSecret} fa fa-eye`}
                aria-hidden="true"
              />
              <p style={{ marginLeft: 6 }}>Show Connection String</p>
            </span>
          )}
          {showUrl && (
            <ToolTip
              id="connection-string-hide"
              placement="right"
              message="Hide connection string"
            >
              <i
                className={`${styles.closeHeader} fa fa-times`}
                aria-hidden="true"
                onClick={() => setShowUrl(false)}
                style={{ paddingLeft: 10 }}
              />
            </ToolTip>
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
      <div className={styles.flexColumn}>
        <h5 className={styles.read_replicas_heading}>Read Replicas</h5>
        <p>
          Hasura Cloud can load balance queries and subscriptions across read
          replicas while sending all mutations and metadata API calls to the
          master.&nbsp;
          <a
            href="https://hasura.io/docs/latest/graphql/cloud/read-replicas.html"
            target="_blank"
            rel="noopener noreferrer"
          >
            <i>(Read More)</i>
          </a>
        </p>
        {readReplicaState.map(stateVar => (
          <ReadReplicaListItem
            currentState={stateVar}
            onClickRemove={onClickRemoveReadReplica(stateVar.displayName)}
          />
        ))}
        {!isReadReplicaButtonClicked ? (
          <Button
            onClick={onClickAddReadReplica}
            className={styles.add_button_styles}
          >
            Add Read Replica
          </Button>
        ) : (
          <Form
            connectDBState={connectDBState}
            connectDBStateDispatch={connectDBStateDispatch}
            onClickCancel={onClickCancelOnReadReplica}
            onClickSave={onClickSaveReadReplica}
            readReplicaConnectionType={readReplicaConnectionType}
            updateReadReplicaConnectionType={updateReadReplicaConnectionType}
          />
        )}
      </div>
      <hr className={styles.line_width} />
    </>
  );
};

export default ReadReplicaForm;
