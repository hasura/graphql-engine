import React from 'react';
import { Link } from 'react-router';
import { FaSearch } from 'react-icons/fa';

import { Button } from '../../../../new-components/Button';
import { Analytics } from '../../../../features/Analytics';
import styles from './LeftSubSidebar.module.scss';

interface Props extends React.ComponentProps<'div'> {
  showAddBtn: boolean;
  searchInput: React.ReactNode;
  heading: string;
  addLink: string;
  addLabel: string;
  addTrackId: string;
  addTestString: string;
  childListTestString: string;
  /* padding addBtn override the default "create" button
  e.g. for action creation in pro console we pass the dropdown button to choose between
  action form and import from OpenAPI
  */
  addBtn?: React.ReactNode;
}

const LeftSubSidebar: React.FC<Props> = props => {
  const {
    showAddBtn,
    searchInput,
    heading,
    addLink,
    addLabel,
    addTrackId,
    addTestString,
    children,
    childListTestString,
    addBtn,
  } = props;

  const getAddButton = () => {
    let addButton = null;

    if (showAddBtn) {
      addButton = (
        <div
          className={`col-xs-4 text-center ${styles.padd_left_remove} ${styles.sidebarCreateTable}`}
        >
          <Link className={styles.padd_remove_full} to={addLink}>
            <Analytics name={addTrackId} passHtmlAttributesToChildren>
              <Button size="sm" mode="default" data-test={addTestString}>
                {addLabel}
              </Button>
            </Analytics>
          </Link>
        </div>
      );
    }

    return addButton;
  };

  return (
    <div className={styles.subSidebarList}>
      <div className={`${styles.display_flex} ${styles.padd_top_medium}`}>
        {searchInput && (
          <div
            className={`${styles.sidebarSearch} form-group col-xs-12 ${styles.padd_remove}`}
          >
            <FaSearch aria-hidden="true" />
            {searchInput}
          </div>
        )}
      </div>
      <div>
        <div className={styles.sidebarHeadingWrapper}>
          <div
            className={`col-xs-8 ${styles.sidebarHeading} ${styles.padd_left_remove}`}
          >
            {heading}
          </div>
          {addBtn ?? getAddButton()}
        </div>
        <ul className={styles.subSidebarListUL} data-test={childListTestString}>
          {children}
        </ul>
      </div>
    </div>
  );
};

export default LeftSubSidebar;
