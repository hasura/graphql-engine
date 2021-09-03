import React, { useState, useEffect } from 'react';
import { connect, ConnectedProps } from 'react-redux';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';
import Helmet from 'react-helmet';
import { MapStateToProps } from '../../../../../types';
import { useEventTrigger } from '../state';
import styles from '../TableCommon/EventTable.scss';
import DropdownButton from '../../../../Common/DropdownButton/DropdownButton';
import Headers from '../../../../Common/Headers/Headers';
import CollapsibleToggle from '../../../../Common/CollapsibleToggle/CollapsibleToggle';
import Button from '../../../../Common/Button/Button';
import { createEventTrigger } from '../../ServerIO';
import Operations from '../Common/Operations';
import RetryConfEditor from '../../Common/Components/RetryConfEditor';
import * as tooltip from '../Common/Tooltips';
import { EVENTS_SERVICE_HEADING } from '../../constants';
import { mapDispatchToPropsEmpty } from '../../../../Common/utils/reactUtils';
import { getDataSources } from '../../../../../metadata/selector';
import { DataSource } from '../../../../../metadata/types';
import { getDatabaseSchemasInfo } from '../../../Data/DataActions';
import { getSourceDriver } from '../../../Data/utils';
import {
  currentDriver,
  setDriver,
  getSupportedDrivers,
  isFeatureSupported,
} from '../../../../../dataSources';

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
    source,
  } = state;
  const { dispatch, readOnlyMode, dataSourcesList, currentDataSource } = props;

  useEffect(() => {
    setState.source(currentDataSource);
    const driver = getSourceDriver(dataSourcesList, currentDataSource);
    if (isFeatureSupported('events.triggers.enabled')) {
      setDriver(driver);
    }
  }, [currentDataSource, dataSourcesList]);

  const [databaseInfo, setDatabaseInfo] = useState<{
    [schema_name: string]: { [table_name: string]: string[] };
  }>({});

  const supportedDrivers = getSupportedDrivers('events.triggers.add');

  useEffect(() => {
    dispatch(
      getDatabaseSchemasInfo(currentDriver, source || currentDataSource) as any
    ).then(setDatabaseInfo);
  }, [currentDataSource, source, dispatch]);

  useEffect(() => {
    if (source && table.schema && table.name) {
      setState.operationColumns(
        databaseInfo[table.schema][table.name].map(c => {
          return {
            name: c,
            enabled: true,
            type: '', // todo — update types, make it optional
          };
        })
      );
    }
  }, [table.name, source, table.schema, databaseInfo]);

  const createBtnText = 'Create Event Trigger';

  const submit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    dispatch(createEventTrigger(state));
  };

  const handleTriggerNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const triggerName = e.target.value;
    setState.name(triggerName);
  };

  const handleDatabaseChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    const database = e.target.value;
    setState.source(database);
    dispatch(getDatabaseSchemasInfo('postgres', database) as any).then(
      setDatabaseInfo
    );
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
    if (!table.name) {
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
      const { name: colName, enabled: colEnabled } = opCol;

      return (
        <div
          key={opCol.name}
          className={`${styles.padd_remove} ${styles.columnListElement}`}
        >
          <div className="checkbox ">
            <label className={styles.cursorPointer}>
              <input
                type="checkbox"
                checked={colEnabled}
                onChange={handleToggleColumn}
                className={`${styles.cursorPointer} legacy-input-fix`}
              />
              {colName}
              {/* <small> ({colType})</small> TODO */}
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
              maxLength={42}
            />
            <hr className="my-md" />
            <h4 className={styles.subheading_text}>
              Database &nbsp; &nbsp;
              <OverlayTrigger
                placement="right"
                overlay={tooltip.triggerNameSource}
              >
                <i className="fa fa-question-circle" aria-hidden="true" />
              </OverlayTrigger>
            </h4>
            <select
              className={`${styles.select} form-control ${styles.add_pad_left}`}
              onChange={handleDatabaseChange}
              data-test="select-source"
              value={source}
            >
              <option value="">Select database</option>
              {dataSourcesList
                .filter(s => supportedDrivers.includes(s.driver))
                .map(s => (
                  <option key={s.name} value={s.name}>
                    {s.name}
                  </option>
                ))}
            </select>
            <hr className="my-md" />
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
              value={table.schema}
            >
              <option value="">Select schema</option>
              {Object.keys(databaseInfo).map(s => (
                <option value={s} key={s}>
                  {s}
                </option>
              ))}
            </select>
            <select
              onChange={handleTableChange}
              data-test="select-table"
              required
              className={`${styles.selectTrigger} form-control ${styles.add_mar_left}`}
              value={table.name}
            >
              <option value="">Select table</option>
              {databaseInfo[table.schema] &&
                Object.keys(databaseInfo[table.schema]).map(t => {
                  return (
                    <option key={t} value={t}>
                      {t}
                    </option>
                  );
                })}
            </select>
            <hr className="my-md" />
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
            <hr className="my-md" />
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
            <hr className="my-md" />
            <CollapsibleToggle
              title={
                <h4 className={styles.subheading_text}>Advanced Settings</h4>
              }
              testId="advanced-settings"
            >
              <div>
                {advancedColumnSection}
                <hr className="my-md" />
                <div className={styles.add_mar_top}>
                  <h4 className={styles.subheading_text}>Retry Logic</h4>
                  <RetryConfEditor
                    retryConf={retryConf}
                    setRetryConf={setState.retryConf}
                  />
                </div>
                <hr className="my-md" />
                <div className={styles.add_mar_top}>
                  <h4 className={styles.subheading_text}>Headers</h4>
                  {headersList}
                </div>
              </div>
            </CollapsibleToggle>
            <hr className="my-md" />
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
  readOnlyMode: boolean;
  dataSourcesList: DataSource[];
  currentDataSource: string;
};

const mapStateToProps: MapStateToProps<PropsFromState> = state => {
  return {
    readOnlyMode: state.main.readOnlyMode,
    dataSourcesList: getDataSources(state),
    currentDataSource: state.tables.currentDataSource,
  };
};

const connector = connect(mapStateToProps, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const AddConnector = connector(Add);
export default AddConnector;
