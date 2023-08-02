import React, { ReactElement } from 'react';
import { FaCopy, FaTrash } from 'react-icons/fa';
import { Button } from '../../../../new-components/Button';
import Header from './Header';
import styles from './PermissionsSummary.module.scss';

type IconButtonProps = {
  onClick: (e: React.MouseEvent<HTMLButtonElement>) => void;
  icon: ReactElement;
  title: string;
};

const IconButton: React.FC<IconButtonProps> = ({ onClick, icon, title }) => (
  <Button size="sm" onClick={onClick} title={title} icon={icon} />
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
              <IconButton
                icon={
                  <FaCopy className={styles.actionIcon} aria-hidden="true" />
                }
                onClick={e => onCopyClick(e, role)}
                title="Copy Permissions"
              />,
              <IconButton
                icon={
                  <FaTrash className={styles.actionIcon} aria-hidden="true" />
                }
                onClick={e => onDeleteClick(e, role)}
                title="Delete Role Permissions"
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
