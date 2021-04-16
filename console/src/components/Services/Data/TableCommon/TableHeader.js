import React from 'react';
import { Link } from 'react-router';
import Helmet from 'react-helmet';
import { changeTableName } from '../TableModify/ModifyActions';
import { capitalize, exists } from '../../../Common/utils/jsUtils';
import EditableHeading from '../../../Common/EditableHeading/EditableHeading';
import BreadCrumb from '../../../Common/Layout/BreadCrumb/BreadCrumb';
import { tabNameMap } from '../utils';
import {
  currentDriver,
  dataSource,
  isFeatureSupported,
} from '../../../../dataSources';
import {
  getSchemaBaseRoute,
  getTableBrowseRoute,
  getTableEditRowRoute,
  getTableInsertRowRoute,
  getTableModifyRoute,
  getTablePermissionsRoute,
  getTableRelationshipsRoute,
  getDataSourceBaseRoute,
} from '../../../Common/utils/routesUtils';
import { getReadableNumber } from '../../../Common/utils/jsUtils';

const TableHeader = ({
  tabName,
  count,
  isCountEstimated,
  table,
  migrationMode,
  readOnlyMode,
  source,
  dispatch,
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

  const getBreadCrumbs = () => {
    return [
      {
        title: 'Data',
        url: '/data',
      },
      {
        title: source,
        url: getDataSourceBaseRoute(source),
        prefix: <i className="fa fa-database" />,
      },
      {
        title: tableSchema,
        url: getSchemaBaseRoute(tableSchema, source),
        prefix: <i className="fa fa-folder" />,
      },
      {
        title: tableName,
        url: getTableBrowseRoute(tableSchema, source, tableName, isTableType),
        prefix: <i className="fa fa-table" />,
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

  return (
    <div>
      <Helmet
        title={capitalize(tabName) + ' - ' + tableName + ' - Data | Hasura'}
      />
      <div className={styles.subHeader}>
        <BreadCrumb breadCrumbs={getBreadCrumbs()} />
        <EditableHeading
          currentValue={tableName}
          save={saveTableNameChange}
          loading={false}
          editable={tabName === 'modify' && currentDriver === 'postgres'}
          dispatch={dispatch}
          property={isTableType ? 'table' : 'view'}
        />
        <div className={styles.nav}>
          <ul className="nav nav-pills">
            {getTab(
              'browse',
              getTableBrowseRoute(tableSchema, source, tableName, isTableType),
              `Browse Rows ${
                isFeatureSupported('tables.browse.aggregation')
                  ? countDisplay
                  : ''
              }`,
              'table-browse-rows'
            )}
            {isFeatureSupported('tables.insert.enabled') &&
              !readOnlyMode &&
              isTableType &&
              getTab(
                'insert',
                getTableInsertRowRoute(
                  tableSchema,
                  source,
                  tableName,
                  isTableType
                ),
                'Insert Row',
                'table-insert-rows'
              )}
            {isFeatureSupported('tables.modify.enabled') &&
              migrationMode &&
              getTab(
                'modify',
                getTableModifyRoute(
                  tableSchema,
                  source,
                  tableName,
                  isTableType
                ),
                'Modify'
              )}
            {getTab(
              'relationships',
              getTableRelationshipsRoute(
                tableSchema,
                source,
                tableName,
                isTableType
              ),
              'Relationships'
            )}
            {getTab(
              'permissions',
              getTablePermissionsRoute(
                tableSchema,
                source,
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
                  source,
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
export default TableHeader;
