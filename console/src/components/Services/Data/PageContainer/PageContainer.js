/* eslint-disable no-unused-vars */

import React from 'react';
import { connect } from 'react-redux';
import { Link } from 'react-router';
import globals from '../../../../Globals';
import Button from '../../Layout/Button/Button';

import { LISTING_SCHEMA, UPDATE_TRACKED_FUNCTIONS } from '../DataActions';
import semverCheck from '../../../../helpers/semver';

const appPrefix = '/data';

const PageContainer = ({
  schema,
  listingSchema,
  functionsList,
  listedFunctions,
  currentTable,
  schemaName,
  migrationMode,
  children,
  dispatch,
  location,
  currentFunction,
  serverVersion,
}) => {
  const styles = require('./PageContainer.scss');
  const functionSymbol = require('./function.svg');
  const functionSymbolActive = require('./function_high.svg');

  const handleFunc = semverCheck('customFunctionSection', serverVersion)
    ? true
    : false;
  // Now schema might be null or an empty array
  let tableLinks = [
    <li className={styles.noTables} key="no-tables-1">
      <i>No tables/views available</i>
    </li>,
  ];
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
              data-test={tableName}
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

  const dividerHr = [
    <li key={'fn-divider-1'}>
      <hr className={styles.tableFunctionDivider} />
    </li>,
  ];

  // If the listedFunctions is non empty
  if (listedFunctions.length > 0) {
    const functionHtml = listedFunctions.map((f, i) => (
      <li
        className={
          f.function_name === currentFunction ? styles.activeTable : ''
        }
        key={'fn ' + i}
      >
        <Link
          to={
            appPrefix +
            '/schema/' +
            schemaName +
            '/functions/' +
            f.function_name
          }
          data-test={f.function_name}
        >
          <div className={styles.display_inline + ' ' + styles.functionIcon}>
            <img
              src={
                f.function_name === currentFunction
                  ? functionSymbolActive
                  : functionSymbol
              }
            />
          </div>
          {f.function_name}
        </Link>
      </li>
    ));

    tableLinks = [...tableLinks, ...dividerHr, ...functionHtml];
  } else if (
    functionsList.length !== listedFunctions.length &&
    listedFunctions.length === 0
  ) {
    const noFunctionResult = [
      <li className={styles.noTables}>
        <i>No matching functions available</i>
      </li>,
    ];
    tableLinks = [...tableLinks, ...dividerHr, ...noFunctionResult];
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

    const matchedFuncs = functionsList.filter(
      f => f.function_name.indexOf(searchTerm) !== -1
    );

    // update schema with matchedTables
    dispatch({ type: LISTING_SCHEMA, updatedSchemas: matchedTables });
    dispatch({ type: UPDATE_TRACKED_FUNCTIONS, data: matchedFuncs });
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
            placeholder={`search table/view${handleFunc ? '/function' : ''}`}
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
            Tables ({schema.length})
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
                <Button
                  className={styles.add_mar_right}
                  size="xs"
                  color="white"
                  data-test="sidebar-add-table"
                >
                  Add Table
                </Button>
              </Link>
            </div>
          ) : null}
        </div>
        <ul className={styles.schemaListUl} data-test="table-links">
          {tableLinks}
        </ul>
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
    functionsList: state.tables.trackedFunctions,
    listedFunctions: state.tables.listedFunctions,
    currentFunction: state.functions.functionName,
    serverVersion: state.main.serverVersion ? state.main.serverVersion : '',
  };
};

export default connect(mapStateToProps)(PageContainer);
