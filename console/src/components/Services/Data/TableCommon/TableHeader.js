import React from 'react';
import globals from '../../../../Globals';
import { Link } from 'react-router';
import Helmet from 'react-helmet';
import { changeTableOrViewName } from '../TableModify/ModifyActions';
import EditableHeading from '../../../Common/EditableHeading/EditableHeading';
import { tabNameMap } from '../utils';

const TableHeader = ({
  tableName,
  tabName,
  count,
  migrationMode,
  currentSchema,
  dispatch,
  allowRename,
}) => {
  const styles = require('../../../Common/TableCommon/Table.scss');
  let capitalised = tabName;
  capitalised = capitalised[0].toUpperCase() + capitalised.slice(1);
  let showCount = '';
  if (!(count === null || count === undefined)) {
    showCount = '(' + count + ')';
  }
  const activeTab = tabNameMap[tabName];

  const tableRenameCallback = newName => {
    const currentPath = window.location.pathname.replace(
      new RegExp(globals.urlPrefix, 'g'),
      ''
    );
    const newPath = currentPath.replace(
      /(\/schema\/.*)\/tables\/(\w*)(\/.*)?/,
      `$1/tables/${newName}$3`
    );
    window.location.replace(
      `${window.location.origin}${globals.urlPrefix}${newPath}`
    );
  };

  const saveTableNameChange = newName => {
    dispatch(
      changeTableOrViewName(true, tableName, newName, () =>
        tableRenameCallback(newName)
      )
    );
  };

  return (
    <div>
      <Helmet title={capitalised + ' - ' + tableName + ' - Data | Hasura'} />
      <div className={styles.subHeader}>
        <div className={styles.dataBreadCrumb}>
          You are here: <Link to={'/data/schema/' + currentSchema}>Data</Link>{' '}
          <i className="fa fa-angle-right" aria-hidden="true" />{' '}
          <Link to={'/data/schema/' + currentSchema}>Schema</Link>{' '}
          <i className="fa fa-angle-right" aria-hidden="true" />{' '}
          <Link to={'/data/schema/' + currentSchema}>{currentSchema}</Link>{' '}
          <i className="fa fa-angle-right" aria-hidden="true" />{' '}
          <Link
            to={
              '/data/schema/' +
              currentSchema +
              '/tables/' +
              tableName +
              '/browse'
            }
          >
            {tableName}
          </Link>{' '}
          <i className="fa fa-angle-right" aria-hidden="true" /> {activeTab}
        </div>
        <EditableHeading
          currentValue={tableName}
          save={saveTableNameChange}
          loading={false}
          editable={tabName === 'modify' && allowRename}
          dispatch={dispatch}
          property="table"
        />
        <div className={styles.nav}>
          <ul className="nav nav-pills">
            <li
              role="presentation"
              className={tabName === 'browse' ? styles.active : ''}
            >
              <Link
                to={
                  '/data/schema/' +
                  currentSchema +
                  '/tables/' +
                  tableName +
                  '/browse'
                }
                data-test="table-browse-rows"
              >
                Browse Rows {showCount}
              </Link>
            </li>
            <li
              role="presentation"
              className={tabName === 'insert' ? styles.active : ''}
            >
              <Link
                to={
                  '/data/schema/' +
                  currentSchema +
                  '/tables/' +
                  tableName +
                  '/insert'
                }
                data-test="table-insert-rows"
              >
                Insert Row
              </Link>
            </li>
            {migrationMode ? (
              <li
                role="presentation"
                className={tabName === 'modify' ? styles.active : ''}
              >
                <Link
                  to={
                    '/data/schema/' +
                    currentSchema +
                    '/tables/' +
                    tableName +
                    '/modify'
                  }
                  data-test="table-modify"
                >
                  Modify
                </Link>
              </li>
            ) : null}
            <li
              role="presentation"
              className={tabName === 'relationships' ? styles.active : ''}
            >
              <Link
                to={
                  '/data/schema/' +
                  currentSchema +
                  '/tables/' +
                  tableName +
                  '/relationships'
                }
                data-test="table-relationships"
              >
                Relationships
              </Link>
            </li>
            <li
              role="presentation"
              className={tabName === 'permissions' ? styles.active : ''}
            >
              <Link
                to={
                  '/data/schema/' +
                  currentSchema +
                  '/tables/' +
                  tableName +
                  '/permissions'
                }
                data-test="table-permissions"
              >
                Permissions
              </Link>
            </li>
          </ul>
        </div>
        <div className="clearfix" />
      </div>
    </div>
  );
};
export default TableHeader;
