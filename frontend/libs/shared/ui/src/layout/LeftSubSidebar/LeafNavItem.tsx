import { ReactNode } from 'react';
import { To } from 'react-router';
import { RelativeLink } from '../../components';

export type LeftSidebarLeafNavItemProps = {
  isActive: boolean;
  children: ReactNode;
  to: To;
};

export const LeftSidebarLeafNavItem = ({
  isActive,
  children,
  to,
}: LeftSidebarLeafNavItemProps) => {
  return (
    <RelativeLink
      to={to}
      className="block! w-full!"
      color={isActive ? 'indigo' : 'gray'}
      size="2"
    >
      {children}
    </RelativeLink>
  );
};
