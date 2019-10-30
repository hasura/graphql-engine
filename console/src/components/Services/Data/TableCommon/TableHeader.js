import React from 'react';
import { Link } from 'react-router';
import Helmet from 'react-helmet';
import { changeTableName } from '../TableModify/ModifyActions';
import EditableHeading from '../../../Common/EditableHeading/EditableHeading';
import BreadCrumb from '../../../Common/Layout/BreadCrumb/BreadCrumb';
import { tabNameMap } from '../utils';
import {
  checkIfTable,
  getTableName,
  getTableSchema,
} from '../../../Common/utils/pgUtils';
import {
  getSchemaBaseRoute,
  getTableBrowseRoute,
  getTableEditRowRoute,
  getTableInsertRowRoute,
  getTableModifyRoute,
  getTablePermissionsRoute,
  getTableRelationshipsRoute,
} from '../../../Common/utils/routesUtils';

const TableHeader = ({ tabName, count, table, migrationMode, dispatch }) => {
  const styles = require('../../../Common/TableCommon/Table.scss');

  const capitalisedTabName = tabName[0].toUpperCase() + tabName.slice(1);

  const tableName = getTableName(table);
  const tableSchema = getTableSchema(table);
  const isTable = checkIfTable(table);

  let countDisplay = '';
  if (!(count === null || count === undefined)) {
    countDisplay = '(' + count + ')';
  }
  const activeTab = tabNameMap[tabName];

  const saveTableNameChange = newName => {
    dispatch(changeTableName(tableName, newName, isTable));
  };

  const getBreadCrumbs = () => {
    return [
      {
        title: 'Data',
        url: '/data',
      },
      {
        title: 'Schema',
        url: '/data/schema/',
      },
      {
        title: tableSchema,
        url: getSchemaBaseRoute(tableSchema),
      },
      {
        title: tableName,
        url: getTableBrowseRoute(table),
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
        title={capitalisedTabName + ' - ' + tableName + ' - Data | Hasura'}
      />
      <div className={styles.subHeader}>
        <BreadCrumb breadCrumbs={getBreadCrumbs()} />
        <EditableHeading
          currentValue={tableName}
          save={saveTableNameChange}
          loading={false}
          editable={tabName === 'modify'}
          dispatch={dispatch}
          property={isTable ? 'table' : 'view'}
        />
        <div className={styles.nav}>
          <ul className="nav nav-pills">
            {getTab(
              'browse',
              getTableBrowseRoute(table),
              `Browse Rows ${countDisplay}`,
              'table-browse-rows'
            )}
            {isTable &&
              getTab(
                'insert',
                getTableInsertRowRoute(table),
                'Insert Row',
                'table-insert-rows'
              )}
            {migrationMode &&
              getTab('modify', getTableModifyRoute(table), 'Modify')}
            {getTab(
              'relationships',
              getTableRelationshipsRoute(table),
              'Relationships'
            )}
            {getTab(
              'permissions',
              getTablePermissionsRoute(table),
              'Permissions'
            )}
            {tabName === 'edit' &&
              getTab('edit', getTableEditRowRoute(table), 'Edit Row')}
          </ul>
        </div>
        <div className="clearfix" />
      </div>
    </div>
  );
};
export default TableHeader;
