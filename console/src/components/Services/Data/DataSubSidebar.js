import React from 'react';
import { connect } from 'react-redux';
import { Link } from 'react-router';

import LeftSubSidebar from '../../Common/Layout/LeftSubSidebar/LeftSubSidebar';

const appPrefix = '/data';

class DataSubSidebar extends React.Component {
  constructor() {
    super();

    this.tableSearch = this.tableSearch.bind(this);
    this.state = {
      trackedTables: [],
      searchInput: '',
    };
  }

  static getDerivedStateFromProps(props) {
    const { currentSchema, schema } = props;

    const trackedTables = schema.filter(
      table => table.is_table_tracked && table.table_schema === currentSchema
    );

    return {
      trackedTables: trackedTables,
    };
  }

  shouldComponentUpdate(nextProps) {
    if (nextProps.metadata.ongoingRequest) {
      return false;
    }
    return true;
  }

  tableSearch(e) {
    const searchTerm = e.target.value;

    this.setState({
      searchInput: searchTerm,
    });
  }

  render() {
    const styles = require('../../Common/Layout/LeftSubSidebar/LeftSubSidebar.scss');
    const functionSymbol = require('../../Common/Layout/LeftSubSidebar/function.svg');
    const functionSymbolActive = require('../../Common/Layout/LeftSubSidebar/function_high.svg');
    const {
      functionsList,
      currentTable,
      currentSchema,
      migrationMode,
      location,
      currentFunction,
    } = this.props;

    const { trackedTables, searchInput } = this.state;

    const trackedTablesLength = trackedTables.length;

    const tableList = trackedTables.filter(t =>
      t.table_name.includes(searchInput)
    );

    const listedFunctions = functionsList.filter(f =>
      f.function_name.includes(searchInput)
    );

    const getSearchInput = () => {
      return (
        <input
          type="text"
          onChange={this.tableSearch}
          className="form-control"
          placeholder={'search table/view/function'}
          data-test="search-tables"
        />
      );
    };

    const getChildList = () => {
      let tableLinks = [
        <li className={styles.noChildren} key="no-tables-1">
          <i>No tables/views available</i>
        </li>,
      ];

      const tables = {};
      tableList.map(t => {
        if (t.is_table_tracked) {
          tables[t.table_name] = t;
        }
      });

      const currentLocation = location.pathname;

      if (tableList && tableList.length) {
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
            if (tables[tableName].table_type === 'BASE TABLE') {
              return (
                <li className={activeTableClass} key={i}>
                  <Link
                    to={
                      appPrefix +
                      '/schema/' +
                      currentSchema +
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
                    currentSchema +
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
                currentSchema +
                '/functions/' +
                f.function_name
              }
              data-test={f.function_name}
            >
              <div
                className={styles.display_inline + ' ' + styles.functionIcon}
              >
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
          <li className={styles.noChildren}>
            <i>No matching functions available</i>
          </li>,
        ];

        tableLinks = [...tableLinks, ...dividerHr, ...noFunctionResult];
      }

      return tableLinks;
    };

    return (
      <LeftSubSidebar
        showAddBtn={migrationMode}
        searchInput={getSearchInput()}
        heading={`Tables (${trackedTablesLength})`}
        addLink={'/data/schema/' + currentSchema + '/table/add'}
        addLabel={'Add Table'}
        addTestString={'sidebar-add-table'}
        childListTestString={'table-links'}
      >
        {getChildList()}
      </LeftSubSidebar>
    );
  }
}

const mapStateToProps = state => {
  return {
    currentTable: state.tables.currentTable,
    migrationMode: state.main.migrationMode,
    functionsList: state.tables.trackedFunctions,
    currentFunction: state.functions.functionName,
    serverVersion: state.main.serverVersion ? state.main.serverVersion : '',
    metadata: state.metadata,
  };
};

export default connect(mapStateToProps)(DataSubSidebar);
