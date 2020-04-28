import React from 'react';
import TableHeader from '../TableCommon/TableHeader';
import styles from './ModifyEvent.scss';

import { getTableColumns } from '../utils';
import { updateSchemaInfo } from '../../../Data/DataActions';
import { findTable, generateTableDef } from '../../../../Common/utils/pgUtils';

import Info from './Info';
import WebhookEditor from './WebhookEditor';
import OperationEditor from './OperationEditor';
import RetryConfEditor from './RetryConfEditor';
import HeadersEditor from './HeadersEditor';
import ActionButtons from './ActionButtons';

import { save, setDefaults, RESET_MODIFY_STATE } from './Actions';

import { NotFoundError } from '../../../Error/PageNotFound';

class Modify extends React.Component {
  componentDidMount() {
    const { dispatch, currentTrigger } = this.props;
    dispatch(setDefaults());
    dispatch(
      updateSchemaInfo({
        tables: [
          {
            table_name: currentTrigger.table_name,
            table_schema: currentTrigger.table_schema,
          },
        ],
      })
    );
  }

  componentWillUnmount() {
    const { dispatch } = this.props;
    dispatch({
      type: RESET_MODIFY_STATE,
    });
  }

  render() {
    const {
      modifyTriggerName,
      modifyTrigger,
      readOnlyMode,
      dispatch,
      allSchemas,
      currentTrigger,
    } = this.props;

    if (!currentTrigger) {
      // throw a 404 exception
      throw new NotFoundError();
    }

    const {
      definition,
      headers,
      webhook,
      webhook_from_env,
      retry_conf,
    } = currentTrigger.configuration;

    const triggerTableSchema = findTable(
      allSchemas,
      generateTableDef(currentTrigger.table_name, currentTrigger.table_schema)
    );

    return (
      <div className={styles.containerWhole + ' container-fluid'}>
        <TableHeader
          dispatch={dispatch}
          triggerName={modifyTriggerName}
          tabName="modify"
          readOnlyMode={readOnlyMode}
        />
        <br />
        <div className={styles.container}>
          <Info
            triggerName={currentTrigger.name}
            tableName={currentTrigger.table_name}
            schemaName={currentTrigger.table_schema}
            styles={styles}
          />
          <WebhookEditor
            webhook={webhook || webhook_from_env}
            dispatch={dispatch}
            modifyTrigger={modifyTrigger}
            env={Boolean(webhook_from_env)}
            newWebhook={null}
            save={() => dispatch(save('webhook', modifyTriggerName))}
            styles={styles}
          />
          <OperationEditor
            definition={definition}
            allTableColumns={getTableColumns(triggerTableSchema)}
            dispatch={dispatch}
            modifyTrigger={modifyTrigger}
            newDefinition={null}
            styles={styles}
            save={() => dispatch(save('ops', modifyTriggerName))}
          />
          <RetryConfEditor
            retryConf={retry_conf}
            modifyTrigger={modifyTrigger}
            styles={styles}
            save={() => dispatch(save('retry', modifyTriggerName))}
            serverVersion={this.props.serverVersion}
            dispatch={dispatch}
          />
          <HeadersEditor
            headers={headers}
            styles={styles}
            modifyTrigger={modifyTrigger}
            save={() => dispatch(save('headers', modifyTriggerName))}
            dispatch={dispatch}
          />
          <ActionButtons
            styles={styles}
            dispatch={dispatch}
            ongoingRequest={modifyTrigger.ongoingRequest}
            triggerName={modifyTriggerName}
          />
        </div>
        <br />
        <br />
      </div>
    );
  }
}

export default Modify;
