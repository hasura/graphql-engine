/* eslint-disable no-unused-vars */

import React from 'react';
import { connect } from 'react-redux';
import { Link } from 'react-router';
import globals from '../../../../Globals';
import Button from '../../Layout/Button/Button';

import { LISTING_TRIGGER } from '../EventActions';

const appPrefix = '/events';

const PageContainer = ({
  currentTrigger,
  triggerList,
  listingTrigger,
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
  const triggers = {};
  listingTrigger.map(t => {
    triggers[t.name] = t;
  });
  const currentLocation = location.pathname;
  if (listingTrigger && listingTrigger.length) {
    triggerLinks = Object.keys(triggers)
      .sort()
      .map((trigger, i) => {
        let activeTableClass = '';
        if (
          trigger === currentTrigger &&
          currentLocation.indexOf(currentTrigger) !== -1
        ) {
          activeTableClass = styles.activeTable;
        }
        return (
          <li className={activeTableClass} key={i}>
            <Link
              to={appPrefix + '/manage/triggers/' + trigger + '/processed'}
              data-test={trigger}
            >
              <i
                className={styles.tableIcon + ' fa fa-table'}
                aria-hidden="true"
              />
              {trigger}
            </Link>
          </li>
        );
      });
  }

  function triggerSearch(e) {
    const searchTerm = e.target.value;
    // form new schema
    const matchedTables = [];
    triggerList.map(trigger => {
      if (trigger.name.indexOf(searchTerm) !== -1) {
        matchedTables.push(trigger);
      }
    });
    // update schema with matchedTables
    dispatch({ type: LISTING_TRIGGER, updatedList: matchedTables });
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
            onChange={triggerSearch.bind(this)}
            className="form-control"
            placeholder="search triggers"
            data-test="search-triggers"
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
                <Button
                  className={styles.add_mar_right}
                  color="white"
                  size="xs"
                  data-test="sidebar-add-table"
                >
                  Add Trigger
                </Button>
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
    listingTrigger: state.triggers.listingTrigger,
    migrationMode: state.main.migrationMode,
  };
};

export default connect(mapStateToProps)(PageContainer);
