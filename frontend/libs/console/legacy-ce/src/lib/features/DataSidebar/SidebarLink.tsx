import clsx from 'clsx';
import { Link } from 'react-router';
import { styles } from './styles';
import { SidebarLinkType } from './types';

export type SidebarLinkProps = SidebarLinkType & {
  active: boolean;
  dataTest: string;
  onClick?: () => void;
};
export const SidebarLink = ({
  name,
  onClick,
  to,
  dataTest,
  active,
  icon,
}: SidebarLinkProps) => (
  <Link
    to={to}
    className={clsx(
      styles.sideBarItem.default,
      styles.link.default,
      active && `${styles.link.active} ${styles.sideBarItem.active}`
    )}
    data-test={dataTest}
    onClick={onClick}
  >
    {icon}
    {name}
  </Link>
);
