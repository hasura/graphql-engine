import React from 'react';
import { Link } from 'react-router';

import Button from '../../Button/Button';
import styles from './LeftSubSidebar.scss';

interface Props extends React.ComponentProps<'div'> {
  showAddBtn: boolean;
  searchInput: React.ReactNode;
  heading: string;
  addLink: string;
  addLabel: string;
  addTestString: string;
  childListTestString: string;
}

const LeftSubSidebar: React.FC<Props> = props => {
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
        <Link to={addLink}>
          <Button size="xs" color="white" data-test={addTestString}>
            {addLabel}
          </Button>
        </Link>
      )
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
            <i className="fa fa-search" aria-hidden="true" />
            {searchInput}
          </div>
        )}
      </div>
      <div className='flex justify-between'>
        <div className={styles.sidebarHeading}>
          {heading}
        </div>
        {getAddButton()}
      </div>
      <ul className={styles.subSidebarListUL} data-test={childListTestString}>
        {children}
      </ul>
    </div>
  );
};

export default LeftSubSidebar;
