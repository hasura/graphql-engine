import React from 'react';
import { Link } from 'react-router';
import Helmet from 'react-helmet';
import { changeTableOrViewName } from '../TableModify/ModifyActions';
import EditableHeading from '../../../Common/EditableHeading/EditableHeading';
import BreadCrumb from '../../../Common/Layout/BreadCrumb/BreadCrumb';
import { tabNameMap } from '../utils';

const ViewHeader = ({
  tableName,
  tabName,
  currentSchema,
  migrationMode,
  dispatch,
}) => {
  const styles = require('../../../Common/TableCommon/Table.scss');
  let capitalised = tabName;
  capitalised = capitalised[0].toUpperCase() + capitalised.slice(1);
  const activeTab = tabNameMap[tabName];

  const saveViewNameChange = newName => {
    dispatch(changeTableOrViewName(false, tableName, newName));
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
        title: currentSchema,
        url: '/data/schema/' + currentSchema,
      },
      {
        title: tableName,
        url:
          '/data/schema/' + currentSchema + '/views/' + tableName + '/browse',
      },
      {
        title: activeTab,
        url: null,
      },
    ];
  };

  return (
    <div>
      <Helmet title={capitalised + ' - ' + tableName + ' - Data | Hasura'} />
      <div className={styles.subHeader}>
        <BreadCrumb breadCrumbs={getBreadCrumbs()} />
        <EditableHeading
          currentValue={tableName}
          save={saveViewNameChange}
          loading={false}
          editable={tabName === 'modify'}
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
