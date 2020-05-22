import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Helmet from 'react-helmet';
import { MapStateToProps } from '../../../../../types';
import {
  Schema,
  Table,
  findTable,
  getSchemaName,
  getTableName,
} from '../../../../Common/utils/pgUtils';
import { useEventTrigger } from '../state';
import styles from '../TableCommon/EventTable.scss';
import DropdownButton from '../../../../Common/DropdownButton/DropdownButton';
import Headers from '../../../../Common/Headers/Headers';
import CollapsibleToggle from '../../../../Common/CollapsibleToggle/CollapsibleToggle';
import Button from '../../../../Common/Button/Button';
import { updateSchemaInfo } from '../../../Data/DataActions';
import { createEventTrigger } from '../../ServerIO';
import Operations from '../Common/Operations';
import RetryConfEditor from '../../Common/Components/RetryConfEditor';
import * as tooltip from '../Common/Tooltips';
import { EVENTS_SERVICE_HEADING } from '../../constants';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';

interface Props extends InjectedProps {}

const Add: React.FC<Props> = props => {
  const { state, setState } = useEventTrigger();
  const {
    name,
    table,
    operations,
    operationColumns,
    webhook,
    retryConf,
    headers,
  } = state;
  const { dispatch, allSchemas, schemaList, readOnlyMode } = props;

  const selectedTableSchema = findTable(allSchemas, table);

  React.useEffect(() => {
    dispatch(updateSchemaInfo({ schemas: [table.schema] }));
  }, [table.schema]);

  React.useEffect(() => {
    if (selectedTableSchema) {
      setState.operationColumns(
        selectedTableSchema.columns.map(c => {
          return {
            name: c.column_name,
            type: c.data_type,
            enabled: true,
          };
        })
      );
    }
  }, [table.name, allSchemas]);

  const createBtnText = 'Create Event Trigger';

  const submit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    dispatch(createEventTrigger(state));
  };

  const handleTriggerNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const triggerName = e.target.value;
    setState.name(triggerName);
  };

  const handleSchemaChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    const selectedSchemaName = e.target.value;
    setState.table(undefined, selectedSchemaName);
  };

  const handleTableChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    const selectedTableName = e.target.value;
    setState.table(selectedTableName);
  };

  const handleWebhookTypeChange = (e: React.BaseSyntheticEvent) => {
    const type = e.target.getAttribute('value');
    setState.webhook({
      type,
      value: '',
    });
  };

  const handleWebhookValueChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const value = e.target.value;
    setState.webhook({
      type: webhook.type,
      value,
    });
  };

  const getColumnList = () => {
    if (!selectedTableSchema) {
      return <i>Select a table first to get column list</i>;
    }

    return operationColumns.map(opCol => {
      const handleToggleColumn = () => {
        const newCols = operationColumns.map(o => {
          return {
            ...o,
            enabled: o.name === opCol.name ? !o.enabled : o.enabled,
          };
        });
        setState.operationColumns(newCols);
      };
      const { name: colName, type: colType, enabled: colEnabled } = opCol;

      return (
        <div
          key={opCol.name}
          className={`${styles.padd_remove} ${styles.columnListElement}`}
        >
          <div className="checkbox ">
            <label>
              <input
                type="checkbox"
                checked={colEnabled}
                onChange={handleToggleColumn}
              />
              {colName}
              <small> ({colType})</small>
            </label>
          </div>
        </div>
      );
    });
  };

  const advancedColumnSection = (
    <div>
      <h4 className={styles.subheading_text}>
        Listen columns for update &nbsp; &nbsp;
        <OverlayTrigger
          placement="right"
          overlay={tooltip.advancedOperationDescription}
        >
          <i className="fa fa-question-circle" aria-hidden="true" />
        </OverlayTrigger>{' '}
      </h4>
      {operations.update ? (
        <div className={`${styles.clear_fix} ${styles.listenColumnWrapper}`}>
          {getColumnList()}
        </div>
      ) : (
        <div className={`${styles.clear_fix} ${styles.listenColumnWrapper}`}>
          <i>Applicable only if update operation is selected.</i>
        </div>
      )}
    </div>
  );

  const headersList = (
    <Headers headers={headers} setHeaders={setState.headers} />
  );

  return (
    <div
      className={`${styles.addTablesBody} ${styles.clear_fix} ${styles.padd_left}`}
    >
      <Helmet
        title={`Add Event Trigger | ${EVENTS_SERVICE_HEADING} - Hasura`}
      />
      <div className={styles.subHeader}>
        <h2 className={styles.heading_text}>Create a new event trigger</h2>
        <div className="clearfix" />
      </div>
      <br />
      <div className={`container-fluid ${styles.padd_left_remove}`}>
        <form onSubmit={submit}>
          <div
            className={`${styles.addCol} col-xs-12 ${styles.padd_left_remove}`}
          >
            <h4 className={styles.subheading_text}>
              Trigger Name &nbsp; &nbsp;
              <OverlayTrigger
                placement="right"
                overlay={tooltip.triggerNameDescription}
              >
                <i className="fa fa-question-circle" aria-hidden="true" />
              </OverlayTrigger>{' '}
            </h4>
            <input
              type="text"
              data-test="trigger-name"
              placeholder="trigger_name"
              required
              pattern="^[A-Za-z]+[A-Za-z0-9_\\-]*$"
              className={`${styles.tableNameInput} form-control`}
              value={name}
              onChange={handleTriggerNameChange}
            />
            <hr />
            <h4 className={styles.subheading_text}>
              Schema/Table &nbsp; &nbsp;
              <OverlayTrigger
                placement="right"
                overlay={tooltip.postgresDescription}
              >
                <i className="fa fa-question-circle" aria-hidden="true" />
              </OverlayTrigger>{' '}
            </h4>
            <select
              onChange={handleSchemaChange}
              data-test="select-schema"
              className={`${styles.selectTrigger} form-control`}
            >
              {schemaList.map(s => {
                const sName = getSchemaName(s);
                return (
                  <option
                    value={sName}
                    key={sName}
                    selected={sName === table.schema}
                  >
                    {sName}
                  </option>
                );
              })}
            </select>
            <select
              onChange={handleTableChange}
              data-test="select-table"
              required
              className={`${styles.selectTrigger} form-control ${styles.add_mar_left}`}
            >
              <option value="">Select table</option>
              {allSchemas
                .filter(t => t.table_schema === table.schema)
                .map(t => {
                  const tName = getTableName(t);
                  return (
                    <option key={tName} value={tName}>
                      {tName}
                    </option>
                  );
                })}
            </select>
            <hr />
            <div
              className={`${styles.add_mar_bottom} ${styles.selectOperations}`}
            >
              <div
                className={`${styles.add_mar_bottom} ${styles.selectOperations}`}
              >
                <h4 className={styles.subheading_text}>
                  Trigger Operations &nbsp; &nbsp;
                  <OverlayTrigger
                    placement="right"
                    overlay={tooltip.operationsDescription}
                  >
                    <i className="fa fa-question-circle" aria-hidden="true" />
                  </OverlayTrigger>{' '}
                </h4>
                <Operations
                  selectedOperations={operations}
                  setOperations={setState.operations}
                  readOnly={false}
                />
              </div>
            </div>
            <hr />
            <div className={styles.add_mar_bottom}>
              <h4 className={styles.subheading_text}>
                Webhook URL &nbsp; &nbsp;
                <OverlayTrigger
                  placement="right"
                  overlay={tooltip.webhookUrlDescription}
                >
                  <i className="fa fa-question-circle" aria-hidden="true" />
                </OverlayTrigger>{' '}
              </h4>
              <div>
                <div className={styles.dropdown_wrapper}>
                  <DropdownButton
                    dropdownOptions={[
                      { display_text: 'URL', value: 'static' },
                      { display_text: 'From env var', value: 'env' },
                    ]}
                    title={webhook.type === 'static' ? 'URL' : 'From env var'}
                    dataKey={webhook.type === 'static' ? 'static' : 'env'}
                    onButtonChange={handleWebhookTypeChange}
                    onInputChange={handleWebhookValueChange}
                    required
                    bsClass={styles.dropdown_button}
                    inputVal={webhook.value}
                    id="webhook-url"
                    inputPlaceHolder={
                      webhook.type === 'static'
                        ? 'http://httpbin.org/post'
                        : 'MY_WEBHOOK_URL'
                    }
                    testId="webhook"
                  />
                </div>
              </div>
              <br />
              <small>
                Note: Specifying the webhook URL via an environmental variable
                is recommended if you have different URLs for multiple
                environments.
              </small>
            </div>
            <hr />
            <CollapsibleToggle
              title={
                <h4 className={styles.subheading_text}>Advanced Settings</h4>
              }
              testId="advanced-settings"
            >
              <div>
                {advancedColumnSection}
                <hr />
                <div className={styles.add_mar_top}>
                  <h4 className={styles.subheading_text}>Retry Logic</h4>
                  <RetryConfEditor
                    retryConf={retryConf}
                    setRetryConf={setState.retryConf}
                  />
                </div>
                <hr />
                <div className={styles.add_mar_top}>
                  <h4 className={styles.subheading_text}>Headers</h4>
                  {headersList}
                </div>
              </div>
            </CollapsibleToggle>
            <hr />
            {!readOnlyMode && (
              <Button
                type="submit"
                color="yellow"
                size="sm"
                data-test="trigger-create"
              >
                {createBtnText}
              </Button>
            )}
          </div>
        </form>
      </div>
    </div>
  );
};

type PropsFromState = {
  allSchemas: Table[];
  schemaList: Schema[];
  readOnlyMode: boolean;
};

const mapStateToProps: MapStateToProps<PropsFromState> = state => {
  return {
    allSchemas: state.tables.allSchemas,
    schemaList: state.tables.schemaList,
    readOnlyMode: state.main.readOnlyMode,
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const AddConnector = connector(Add);
export default AddConnector;
