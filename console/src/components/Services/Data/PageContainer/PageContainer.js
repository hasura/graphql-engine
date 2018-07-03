/* eslint-disable no-unused-vars */

import React from 'react';
import { connect } from 'react-redux';
import { Link } from 'react-router';
import globals from '../../../../Globals';

import { LISTING_SCHEMA } from '../DataActions';

const appPrefix = '/data';

const PageContainer = ({
  schema,
  listingSchema,
  currentTable,
  schemaName,
  migrationMode,
  children,
  dispatch,
  location,
}) => {
  const styles = require('./PageContainer.scss');
  // Now schema might be null or an empty array
  let tableLinks = (
    <li className={styles.noTables}>
      <i>No tables available</i>
    </li>
  );
  const tables = {};
  listingSchema.map(t => {
    tables[t.table_name] = t;
  });
  const currentLocation = location.pathname;
  if (listingSchema && listingSchema.length) {
    tableLinks = Object.keys(tables)
      .sort()
      .map((tableName, i) => {
        let activeTableClass = '';
        if (
          tableName === currentTable &&
          currentLocation.indexOf(currentTable) !== -1
        ) {
          activeTableClass = styles.activeTable;
        }
        if (tables[tableName].detail.table_type === 'BASE TABLE') {
          return (
            <li className={activeTableClass} key={i}>
              <Link
                to={
                  appPrefix +
                  '/schema/' +
                  schemaName +
                  '/tables/' +
                  tableName +
                  '/browse'
                }
              >
                <i
                  className={styles.tableIcon + ' fa fa-table'}
                  aria-hidden="true"
                  data-test={tableName}
                />
                {tableName}
              </Link>
            </li>
          );
        }
        return (
          <li className={activeTableClass} key={i}>
            <Link
              to={
                appPrefix +
                '/schema/' +
                schemaName +
                '/views/' +
                tableName +
                '/browse'
              }
            >
              <i
                className={styles.tableIcon + ' fa fa-table'}
                aria-hidden="true"
              />
              <i>{tableName}</i>
            </Link>
          </li>
        );
      });
  }

  function tableSearch(e) {
    const searchTerm = e.target.value;
    // form new schema
    const matchedTables = [];
    schema.map(table => {
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
            Tables
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
                to={'/data/schema/' + schemaName + '/table/add'}
              >
                <button
                  className={styles.add_mar_right + ' btn btn-xs btn-default'}
                >
                  Add Table
                </button>
              </Link>
            </div>
          ) : null}
        </div>
        <ul className={styles.schemaListUl}>{tableLinks}</ul>
      </div>
    </div>
  );
};

const mapStateToProps = state => {
  return {
    schemaName: state.tables.currentSchema,
    schema: state.tables.allSchemas,
    listingSchema: state.tables.listingSchemas,
    currentTable: state.tables.currentTable,
    migrationMode: state.main.migrationMode,
  };
};

export default connect(mapStateToProps)(PageContainer);
