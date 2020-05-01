import React from 'react';
import TableHeader from '../TableCommon/TableHeader';
import styles from './ModifyEvent.scss';
import { updateSchemaInfo } from '../../../Data/DataActions';
import { getTableColumns, Table } from '../../../../Common/utils/pgUtils';
import Button from '../../../../Common/Button/Button';
import { useEventTriggerModify } from '../state';

import Info from './Info';
import WebhookEditor from './WebhookEditor';
import OperationEditor from './OperationEditor';
import RetryConfEditor from './RetryConfEditor';
import HeadersEditor from './HeadersEditor';
import { ReduxState } from '../../../../../Types';
import { EventTrigger } from '../../Types';
import { findETTable } from '../../utils';
import { EventTriggerProperty } from './utils';

import { modifyEventTrigger, deleteEventTrigger } from '../../ServerIO';

import { NotFoundError } from '../../../../Error/PageNotFound';

type ModifyProps = {
  currentTrigger: EventTrigger;
  allSchemas: Table[];
  readOnlyMode: boolean;
  dispatch: any;
};

const Modify = (props: ModifyProps) => {
  const { currentTrigger, allSchemas, readOnlyMode, dispatch } = props;

  const { state, setState } = useEventTriggerModify(currentTrigger, allSchemas);

  React.useEffect(() => {
    console.log('Entering effect');
    if (currentTrigger) {
      console.log('inside current trigger true');
      dispatch(
        updateSchemaInfo({
          schemas: [currentTrigger.schema_name],
        })
      );
      console.log('inside current trigger true');
    }
    console.log('Entering effect');
  }, [currentTrigger]);

  const table = findETTable(currentTrigger, allSchemas);

  const saveWrapper = (property: EventTriggerProperty) => (
    successCb?: () => void,
    errorCb?: () => void
  ) => {
    dispatch(
      modifyEventTrigger(
        property,
        state,
        currentTrigger,
        table,
        successCb,
        errorCb
      )
    );
  };

  const deleteWrapper = () => {
    dispatch(deleteEventTrigger(currentTrigger));
  };

  return (
    <div className={`${styles.containerWhole} container-fluid`}>
      <TableHeader
        count={null}
        triggerName={currentTrigger.name}
        tabName="modify"
        readOnlyMode={readOnlyMode}
      />
      <br />
      <div className={styles.container}>
        <Info currentTrigger={currentTrigger} styles={styles} />
        <WebhookEditor
          currentTrigger={currentTrigger}
          webhook={state.webhook}
          setWebhook={setState.webhook}
          save={saveWrapper('webhook')}
          styles={styles}
        />
        <OperationEditor
          currentTrigger={currentTrigger}
          allTableColumns={getTableColumns(
            findETTable(currentTrigger, allSchemas)
          )}
          operations={state.operations}
          setOperations={setState.operations}
          operationColumns={state.operationColumns}
          setOperationColumns={setState.operationColumns}
          styles={styles}
          save={saveWrapper('ops')}
        />
        <RetryConfEditor
          conf={state.retryConf}
          setRetryConf={setState.retryConf}
          currentTrigger={currentTrigger}
          styles={styles}
          save={saveWrapper('retry_conf')}
        />
        <HeadersEditor
          headers={state.headers}
          setHeaders={setState.headers}
          styles={styles}
          currentTrigger={currentTrigger}
          save={saveWrapper('headers')}
        />
        <div className={styles.add_mar_bottom}>
          <Button
            color="red"
            size="sm"
            data-test="delete-trigger"
            onClick={deleteWrapper}
          >
            Delete
          </Button>
        </div>
      </div>
      <br />
      <br />
    </div>
  );
};

const mapStateToProps = (state: ReduxState, ownProps: { params: any }) => {
  const triggerList = state.events.triggers.event;
  const modifyTriggerName = ownProps.params.triggerName;

  const currentTrigger = triggerList.find(tr => tr.name === modifyTriggerName);

  if (!currentTrigger) {
    // throw a 404 exception
    throw new NotFoundError();
  }

  return {
    triggerList,
    currentTrigger,
    allSchemas: state.tables.allSchemas,
    readOnlyMode: state.main.readOnlyMode,
  };
};

const modifyTriggerConnector = (connect: any) =>
  connect(mapStateToProps)(Modify);

export default modifyTriggerConnector;
