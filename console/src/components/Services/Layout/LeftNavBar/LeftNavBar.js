/* eslint-disable no-unused-vars */

import React from 'react';
import { Link } from 'react-router';

import { LISTING_SCHEMA } from '../../Data/DataActions';

const LeftNavBar = ({
  appPrefix,
  listItemTemplate,
  dataList,
  filtered,
  searchQuery,
  location,
  filterItem,
  viewResolver,
  migrationMode,
}) => {
  const styles = require('./LeftNavBar.scss');
  // Now schema might be null or an empty array

  function tableSearch(e) {
    const searchTerm = e.target.value;
    filterItem(dataList, searchTerm);
  }
  // TODO: Make it generic so that other components can use it.

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
            Schemas ({dataList.length})
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
                to={`${appPrefix}/manage/add`}
              >
                <button
                  className={styles.add_mar_right + ' btn btn-xs btn-default'}
                  data-test="sidebar-add-table"
                >
                  Add Schema
                </button>
              </Link>
            </div>
          ) : null}
        </div>
        <ul className={styles.schemaListUl} data-test="table-links">
          {listItemTemplate(
            searchQuery ? filtered : dataList,
            styles,
            location,
            viewResolver
          )}
        </ul>
      </div>
    </div>
  );
};

export default LeftNavBar;
