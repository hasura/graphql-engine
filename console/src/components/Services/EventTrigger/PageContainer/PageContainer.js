/* eslint-disable no-unused-vars */

import React from 'react';
import { connect } from 'react-redux';
import { Link } from 'react-router';
import globals from '../../../../Globals';

import { LISTING_SCHEMA } from '../EventActions';

const appPrefix = '/events';

const PageContainer = ({
  currentTrigger,
  triggerList,
  migrationMode,
  children,
  dispatch,
  location,
}) => {
  const styles = require('./PageContainer.scss');
  // Now schema might be null or an empty array
  let triggerLinks = (
    <li className={styles.noTables}>
      <i>No triggers available</i>
    </li>
  );
  const tables = {};
  const currentLocation = location.pathname;
  if (triggerList && triggerList.length) {
    triggerLinks = Object.keys(tables)
      .sort()
      .map((tableName, i) => {
        let activeTableClass = '';
        if (
          tableName === currentTrigger &&
          currentLocation.indexOf(currentTrigger) !== -1
        ) {
          activeTableClass = styles.activeTable;
        }
        return (
          <li className={activeTableClass} key={i}>
            <Link
              to={
                appPrefix +
                '/events/' +
                '/manage/triggers/' +
                currentTrigger +
                '/processed'
              }
              data-test={tableName}
            >
              <i
                className={styles.tableIcon + ' fa fa-table'}
                aria-hidden="true"
              />
              {tableName}
            </Link>
          </li>
        );
      });
  }

  function tableSearch(e) {
    const searchTerm = e.target.value;
    // form new schema
    const matchedTables = [];
    triggerList.map(table => {
      if (table.table_name.indexOf(searchTerm) !== -1) {
        matchedTables.push(table);
      }
    });
    // update schema with matchedTables
    dispatch({ type: LISTING_SCHEMA, updatedSchemas: matchedTables });
  }

  return (
    <div className={styles.schemaTableList}>
      <div className={styles.display_flex + ' ' + styles.padd_top_medium}>
        <div
          className={
            styles.sidebarSearch + ' form-group col-xs-12 ' + styles.padd_remove
          }
        >
          <i className="fa fa-search" aria-hidden="true" />
          <input
            type="text"
            onChange={tableSearch.bind(this)}
            className="form-control"
            placeholder="search table/view"
            data-test="search-tables"
          />
        </div>
      </div>
      <div>
        <div className={styles.sidebarHeadingWrapper}>
          <div
            className={
              'col-xs-8 ' +
              styles.sidebarHeading +
              ' ' +
              styles.padd_left_remove
            }
          >
            Triggers ({triggerList.length})
          </div>
          {migrationMode ? (
            <div
              className={
                'col-xs-4 text-center ' +
                styles.padd_remove +
                ' ' +
                styles.sidebarCreateTable
              }
            >
              <Link
                className={styles.padd_remove_full}
                to={'/events/manage/triggers/add'}
              >
                <button
                  className={styles.add_mar_right + ' btn btn-xs btn-default'}
                  data-test="sidebar-add-table"
                >
                  Add Trigger
                </button>
              </Link>
            </div>
          ) : null}
        </div>
        <ul className={styles.schemaListUl} data-test="table-links">
          {triggerLinks}
        </ul>
      </div>
    </div>
  );
};

const mapStateToProps = state => {
  return {
    currentTrigger: state.triggers.currentTrigger,
    triggerList: state.triggers.triggerList,
    migrationMode: state.main.migrationMode,
  };
};

export default connect(mapStateToProps)(PageContainer);
