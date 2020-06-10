import React from 'react';
import Button from '../../../Common/Button/Button';
import Header from './Header';
import styles from './PermissionsSummary.scss';

type RolesHeaderProps = {
  selectable?: boolean;
  selectedFirst?: boolean;
  allRoles: Array<string>;
  currentRole: string;
  onCopyClick: (e: React.MouseEvent<HTMLButtonElement>, role: string) => void;
  onDeleteClick: (e: React.MouseEvent<HTMLButtonElement>, role: string) => void;
  setRole: (role: string, isCurrRole: boolean) => void;
};

type IconButtonProps = {
  onClick: (e: React.MouseEvent<HTMLButtonElement>) => void;
  icon: string;
};

const IconButton: React.FC<IconButtonProps> = ({ onClick, icon }) => (
  <Button color="white" size="xs" onClick={onClick} title="Copy permissions">
    <i className={`fa ${icon} ${styles.actionIcon}`} aria-hidden="true" />
  </Button>
);

const RolesHeader: React.FC<RolesHeaderProps> = ({
  selectable = true,
  selectedFirst = false,
  allRoles,
  currentRole,
  onCopyClick,
  onDeleteClick,
  setRole,
}) => {
  const rolesHeaders = [];

  if (!allRoles.length) {
    rolesHeaders.push(<Header content="No roles" selectable={false} />);
  } else {
    allRoles.forEach(role => {
      const isCurrRole = currentRole === role;

      const roleHeader = (
        <Header
          content={role}
          selectable={selectable}
          isSelected={isCurrRole}
          onClick={() => setRole(role, isCurrRole)}
          actionButtons={[
            <IconButton icon="fa-copy" onClick={e => onCopyClick(e, role)} />,
            <IconButton
              icon="fa-trash"
              onClick={e => onDeleteClick(e, role)}
            />,
          ]}
        />
      );

      if (selectedFirst && isCurrRole) {
        rolesHeaders.unshift(roleHeader);
      } else {
        rolesHeaders.push(roleHeader);
      }
    });
  }

  return <>{rolesHeaders}</>;
};

export default RolesHeader;
