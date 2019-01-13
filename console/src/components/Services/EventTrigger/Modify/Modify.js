import React from 'react';
import TableHeader from '../TableCommon/TableHeader';
import styles from './Modify.scss';

import { getTableColumns } from '../utils';
import _push from '../push';

import Info from './Info';
import WebhookEditor from './WebhookEditor';
import OperationEditor from './OperationEditor';
import RetryConfEditor from './RetryConfEditor';
import HeadersEditor from './HeadersEditor';
import ActionButtons from './ActionButtons';

import { save, setDefaults } from './Actions';

class Modify extends React.Component {
  componentDidMount() {
    this.props.dispatch(setDefaults());
  }
  render() {
    const {
      modifyTriggerName,
      modifyTrigger,
      triggerList,
      migrationMode,
      dispatch,
      tableSchemas,
    } = this.props;

    const currentTrigger = triggerList.find(
      tr => tr.name === modifyTriggerName
    );

    if (!currentTrigger) {
      dispatch(_push('/events/manage'));
      return null;
    }

    const {
      definition,
      headers,
      webhook,
      webhook_from_env,
      retry_conf,
    } = currentTrigger.configuration;

    const currentTableSchema = tableSchemas.find(
      tableSchema =>
        tableSchema.table_name === currentTrigger.table_name &&
        tableSchema.table_schema === currentTrigger.schema_name
    );

    return (
      <div className={styles.containerWhole + ' container-fluid'}>
        <TableHeader
          dispatch={dispatch}
          triggerName={modifyTriggerName}
          tabName="modify"
          migrationMode={migrationMode}
        />
        <br />
        <div className={styles.container}>
          <Info
            triggerName={currentTrigger.name}
            tableName={currentTrigger.table_name}
            schemaName={currentTrigger.schema_name}
            triggerId={currentTrigger.id}
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
            allTableColumns={getTableColumns(currentTableSchema)}
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
