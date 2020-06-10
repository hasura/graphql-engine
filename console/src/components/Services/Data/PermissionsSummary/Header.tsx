import React from 'react';
import styles from './PermissionsSummary.scss';

type HeaderProps = {
    content: string;
    selectable: boolean;
    isSelected: boolean;
    onClick: () => void;
    actionButtons: Array<HTMLButtonElement>;
    key: string | null;
};

type HeaderContentProps = {
    content: string;
    actionButtons: Array<HTMLButtonElement>;
};

const HeaderContent: React.FC<HeaderContentProps> = ({ content, actionButtons }) => {
    let headerContent = <></>;

    if (!actionButtons.length) {
        headerContent = <>{content}</>;
    } else {
        headerContent = (
            <div
                className={`${styles.actionCell} ${styles.display_flex} ${styles.flex_space_between}`}
            >
                <div>{content}</div>
                <div
                    className={`${styles.tableHeaderActions} ${styles.display_flex}`}
                >
                    {actionButtons.map((actionButton, i) => (
                        <div key={`${content}-action-btn-${i}`}>{actionButton}</div>
                    ))}
                </div>
            </div>
        );
    }

    return headerContent;
};

const Header: React.FC<HeaderProps> = ({
    content,
    selectable,
    isSelected,
    onClick,
    actionButtons,
    key
}) => {
    const selectableClassName = selectable ? styles.cursorPointer : "";
    const isSelectedClassName = isSelected ? styles.selected : "";

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
