import React from 'react';
import { connect } from 'react-redux';
import { Link } from 'react-router';
// import globals from '../../../Globals';

import LeftSubSidebar from '../../Common/Layout/LeftSubSidebar/LeftSubSidebar';

import { LISTING_TRIGGER } from './EventActions';

const appPrefix = '/events';

const EventSubSidebar = ({
  currentTrigger,
  triggerList,
  listingTrigger,
  // children,
  dispatch,
  location,
}) => {
  const styles = require('../../Common/Layout/LeftSubSidebar/LeftSubSidebar.scss');

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

  const getSearchInput = () => {
    return (
      <input
        type="text"
        onChange={triggerSearch.bind(this)}
        className="form-control"
        placeholder="search event triggers"
        data-test="search-triggers"
      />
    );
  };

  const getChildList = () => {
    let triggerLinks = (
      <li className={styles.noChildren}>
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

    return triggerLinks;
  };

  return (
    <LeftSubSidebar
      showAddBtn
      searchInput={getSearchInput()}
      heading={`Event Triggers (${triggerList.length})`}
      addLink={'/events/manage/triggers/add'}
      addLabel={'Create'}
      addTestString={'sidebar-add-table'}
      childListTestString={'table-links'}
    >
      {getChildList()}
    </LeftSubSidebar>
  );
};

const mapStateToProps = state => {
  return {
    currentTrigger: state.triggers.currentTrigger,
    triggerList: state.triggers.triggerList,
    listingTrigger: state.triggers.listingTrigger,
  };
};

export default connect(mapStateToProps)(EventSubSidebar);
