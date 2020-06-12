import React from 'react';
import Button from '../../../Common/Button/Button';
import Header from './Header';
import styles from './PermissionsSummary.scss';

type IconButtonProps = {
  onClick: (e: React.MouseEvent<HTMLButtonElement>) => void;
  icon: string;
};

const IconButton: React.FC<IconButtonProps> = ({ onClick, icon }) => (
  <Button color="white" size="xs" onClick={onClick} title="Copy permissions">
    <i className={`fa ${icon} ${styles.actionIcon}`} aria-hidden="true" />
  </Button>
);

type RolesHeaderProps = {
  selectable?: boolean;
  selectedFirst?: boolean;
  allRoles: Array<string>;
  currentRole: string;
  onCopyClick: (e: React.MouseEvent<HTMLButtonElement>, role: string) => void;
  onDeleteClick: (e: React.MouseEvent<HTMLButtonElement>, role: string) => void;
  setRole: (role: string, isCurrRole: boolean) => void;
};

const RolesHeader: React.FC<RolesHeaderProps> = ({
  selectable = true,
  selectedFirst = false,
  allRoles,
  currentRole,
  onCopyClick,
  onDeleteClick,
  setRole,
}) => {
  let roles = [...allRoles];
  if (selectedFirst) {
    roles = allRoles.reduce((acc, role) => {
      if (role === currentRole) return [role, ...acc];
      return [...acc, role];
    }, [] as string[]);
  }

  return (
    <>
      {roles.length ? (
        roles.map(role => (
          <Header
            content={role}
            selectable={selectable}
            isSelected={currentRole === role}
            onClick={() => setRole(role, currentRole === role)}
            actionButtons={[
              <IconButton icon="fa-copy" onClick={e => onCopyClick(e, role)} />,
              <IconButton
                icon="fa-trash"
                onClick={e => onDeleteClick(e, role)}
              />,
            ]}
          />
        ))
      ) : (
        <Header content="No roles" selectable={false} />
      )}
    </>
  );
};

export default RolesHeader;
