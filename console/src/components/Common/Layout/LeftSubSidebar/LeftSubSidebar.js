import React from 'react';
import { Link as RouterLink } from 'react-router';

import Button from '../../Button/Button';
import { Icon, Flex } from '../../../UIKit/atoms';
import styles from './LeftSubSidebar.scss';

const LeftSubSidebar = props => {
  const {
    showAddBtn,
    searchInput,
    heading,
    addLink,
    addLabel,
    addTestString,
    children,
    childListTestString,
  } = props;

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
          <RouterLink className={styles.padd_remove_full} to={addLink}>
            <Button size="xs" color="white" data-test={addTestString}>
              {addLabel}
            </Button>
          </RouterLink>
        </div>
      );
    }

    return addButton;
  };

  return (
    <div className={styles.subSidebarList}>
      <Flex pt="10px">
        <div
          className={
            styles.sidebarSearch + ' form-group col-xs-12 ' + styles.padd_remove
          }
        >
          <Icon type="search" color="grey.tab" />
          {searchInput}
        </div>
      </Flex>
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
        <ul className={styles.subSidebarListUL} data-test={childListTestString}>
          {children}
        </ul>
      </div>
    </div>
  );
};

export default LeftSubSidebar;
