import React, { ReactElement } from 'react';
import styles from './PermissionsSummary.scss';

type HeaderContentProps = {
  content: string;
  actionButtons: Array<ReactElement>;
};

const HeaderContent: React.FC<HeaderContentProps> = ({
  content,
  actionButtons,
}) => {
  return actionButtons.length ? (
    <div
      className={`${styles.actionCell} ${styles.display_flex} ${styles.flex_space_between}`}
    >
      <div>{content}</div>
      <div className={`${styles.tableHeaderActions} ${styles.display_flex}`}>
        {actionButtons.map((actionButton, i) => (
          <div key={`${content}-action-btn-${i}`}>{actionButton}</div>
        ))}
      </div>
    </div>
  ) : (
    <>{content}</>
  );
};

type HeaderProps = {
  content: string;
  selectable: boolean;
  isSelected?: boolean;
  onClick?: () => void;
  actionButtons?: Array<ReactElement>;
  key?: string | null;
};

const Header: React.FC<HeaderProps> = ({
  content,
  selectable,
  isSelected,
  onClick,
  actionButtons = [],
  key,
}) => {
  const selectableClassName = selectable ? styles.cursorPointer : '';
  const isSelectedClassName = isSelected ? styles.selected : '';

  return (
    <th
      key={key || content}
      onClick={selectable ? onClick : undefined}
      className={`${selectableClassName} ${isSelectedClassName}`}
    >
      <HeaderContent content={content} actionButtons={actionButtons} />
    </th>
  );
};

export default Header;
