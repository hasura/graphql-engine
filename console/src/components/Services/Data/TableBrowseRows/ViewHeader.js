import React from 'react';
import { Link } from 'react-router';
import Helmet from 'react-helmet';
import globals from '../../../../Globals';
import { changeTableOrViewName } from '../TableModify/ModifyActions';
import EditableHeading from '../../../Common/EditableHeading/EditableHeading';
import { tabNameMap } from '../utils';

const ViewHeader = ({
  tableName,
  tabName,
  currentSchema,
  migrationMode,
  dispatch,
  allowRename,
}) => {
  const styles = require('../../../Common/TableCommon/Table.scss');
  let capitalised = tabName;
  capitalised = capitalised[0].toUpperCase() + capitalised.slice(1);
  const activeTab = tabNameMap[tabName];
  const viewRenameCallback = newName => {
    const currentPath = window.location.pathname.replace(
      new RegExp(globals.urlPrefix, 'g'),
      ''
    );
    const newPath = currentPath.replace(
      /(\/schema\/.*)\/views\/(\w*)(\/.*)?/,
      `$1/views/${newName}$3`
    );
    window.location.replace(
      `${window.location.origin}${globals.urlPrefix}${newPath}`
    );
  };

  const saveViewNameChange = newName => {
    dispatch(
      changeTableOrViewName(false, tableName, newName, () =>
        viewRenameCallback(newName)
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
          <Link
            to={
              '/data/schema/' +
              currentSchema +
              '/views/' +
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
          save={saveViewNameChange}
          loading={false}
          editable={tabName === 'modify' && allowRename}
          dispatch={dispatch}
          property="view"
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
                  '/views/' +
                  tableName +
                  '/browse'
                }
                data-test="table-browse-rows"
              >
                Browse Rows
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
                    '/views/' +
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
                  '/views/' +
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
                  '/views/' +
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
export default ViewHeader;
