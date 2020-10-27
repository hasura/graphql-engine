import React from 'react';
import { Link } from 'react-router';
import Helmet from 'react-helmet';
import { connect } from 'react-redux';

import { changeTableName } from '../TableModify/ModifyActions';
import { capitalize, exists } from '../../../Common/utils/jsUtils';
import EditableHeading from '../../../Common/EditableHeading/EditableHeading';
import BreadCrumb from '../../../Common/Layout/BreadCrumb/BreadCrumb';
import { tabNameMap } from '../utils';
import { dataSource } from '../../../../dataSources';
import {
  getSchemaBaseRoute,
  getTableBrowseRoute,
  getTableEditRowRoute,
  getTableInsertRowRoute,
  getTableModifyRoute,
  getTablePermissionsRoute,
  getTableRelationshipsRoute,
} from '../../../Common/utils/routesUtils';
import { getReadableNumber } from '../../../Common/utils/jsUtils';

const TableHeader = ({
  tabName,
  count,
  isCountEstimated,
  table,
  migrationMode,
  readOnlyMode,
  dispatch,
  ...props
}) => {
  const styles = require('../../../Common/TableCommon/Table.scss');

  const tableName = table.table_name;
  const tableSchema = table.table_schema;
  const isTableType = dataSource.isTable(table);

  let countDisplay = '';
  if (exists(count) && !isCountEstimated) {
    countDisplay = `(${getReadableNumber(count)})`;
  }
  const activeTab = tabNameMap[tabName];

  const saveTableNameChange = newName => {
    dispatch(
      changeTableName(tableName, newName, isTableType, table.table_type)
    );
  };

  const getBreadCrumbs = (currentSource, currentSchema) => {
    return [
      {
        // change
        title: 'Data',
        url: `/data/${currentSource}/schema/${currentSchema}`,
      },
      {
        // change
        title: 'Schema',
        url: `/data/${currentSource}/schema/${currentSchema}`,
      },
      {
        title: tableSchema,
        url: getSchemaBaseRoute(tableSchema, currentSource),
      },
      {
        title: tableName,
        url: getTableBrowseRoute(
          tableSchema,
          currentSource,
          tableName,
          isTableType
        ),
      },
      {
        title: activeTab,
        url: null,
      },
    ];
  };

  const getTab = (tab, link, title, dataTestId) => {
    return (
      <li role="presentation" className={tabName === tab ? styles.active : ''}>
        <Link to={link} data-test={dataTestId || 'table-' + tab}>
          {title}
        </Link>
      </li>
    );
  };

  const crumbs = getBreadCrumbs(props.currentSource, props.currentSchema);
  const { currentSource: currentDataSource } = props;

  return (
    <div>
      <Helmet
        title={capitalize(tabName) + ' - ' + tableName + ' - Data | Hasura'}
      />
      <div className={styles.subHeader}>
        <BreadCrumb breadCrumbs={crumbs} />
        <EditableHeading
          currentValue={tableName}
          save={saveTableNameChange}
          loading={false}
          editable={tabName === 'modify'}
          dispatch={dispatch}
          property={isTableType ? 'table' : 'view'}
        />
        <div className={styles.nav}>
          <ul className="nav nav-pills">
            {getTab(
              'browse',
              getTableBrowseRoute(
                tableSchema,
                currentDataSource,
                tableName,
                isTableType
              ),
              `Browse Rows ${countDisplay}`,
              'table-browse-rows'
            )}
            {!readOnlyMode &&
              isTableType &&
              getTab(
                'insert',
                getTableInsertRowRoute(
                  tableSchema,
                  currentDataSource,
                  tableName,
                  isTableType
                ),
                'Insert Row',
                'table-insert-rows'
              )}
            {migrationMode &&
              getTab(
                'modify',
                getTableModifyRoute(
                  tableSchema,
                  currentDataSource,
                  tableName,
                  isTableType
                ),
                'Modify'
              )}
            {getTab(
              'relationships',
              getTableRelationshipsRoute(
                tableSchema,
                currentDataSource,
                tableName,
                isTableType
              ),
              'Relationships'
            )}
            {getTab(
              'permissions',
              getTablePermissionsRoute(
                tableSchema,
                currentDataSource,
                tableName,
                isTableType
              ),
              'Permissions'
            )}
            {tabName === 'edit' &&
              getTab(
                'edit',
                getTableEditRowRoute(
                  tableSchema,
                  currentDataSource,
                  tableName,
                  isTableType
                ),
                'Edit Row'
              )}
          </ul>
        </div>
        <div className="clearfix" />
      </div>
    </div>
  );
};

const mapStateToProps = state => {
  return {
    currentSource: state.tables.currentDataSource,
    currentSchema: state.tables.currentSchema,
  };
};

const tableHeaderConnector = connect(mapStateToProps);
const ConnectedTableHeader = tableHeaderConnector(TableHeader);

export default ConnectedTableHeader;
