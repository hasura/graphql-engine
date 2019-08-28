import React from 'react';
import { Link } from 'react-router';

import Button from '../../Button/Button';

class LeftSubSidebar extends React.Component {
  render() {
    const styles = require('./LeftSubSidebar.scss');

    const {
      showAddBtn,
      searchInput,
      heading,
      addLink,
      addLabel,
      addTestString,
      children,
      childListTestString,
    } = this.props;

    const getAddButton = () => {
      let addButton = null;

      if (showAddBtn) {
        addButton = (
          <div
            className={
              'col-xs-4 text-center ' +
              styles.padd_left_remove +
              ' ' +
              styles.sidebarCreateTable
            }
          >
            <Link className={styles.padd_remove_full} to={addLink}>
              <Button size="xs" color="white" data-test={addTestString}>
                {addLabel}
              </Button>
            </Link>
          </div>
        );
      }

      return addButton;
    };

    return (
      <div className={styles.subSidebarList}>
        <div className={styles.display_flex + ' ' + styles.padd_top_medium}>
          <div
            className={
              styles.sidebarSearch +
              ' form-group col-xs-12 ' +
              styles.padd_remove
            }
          >
            <i className="fa fa-search" aria-hidden="true" />
            {searchInput}
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
              {heading}
            </div>
            {getAddButton()}
          </div>
          <ul
            className={styles.subSidebarListUL}
            data-test={childListTestString}
          >
            {children}
          </ul>
        </div>
      </div>
    );
  }
}

export default LeftSubSidebar;
