import clsx from 'clsx';
import { Link } from 'react-router';
import { styles } from './styles';
import { SidebarLinkType } from './types';
import Skeleton from 'react-loading-skeleton';

export type SidebarLinkProps = SidebarLinkType & {
  active: boolean;
  dataTest: string;
  onClick?: () => void;
  isLoading: boolean;
};
export const SidebarLink = ({
  name,
  onClick,
  to,
  dataTest,
  active,
  icon,
  isLoading,
}: SidebarLinkProps) => (
  <Link
    className={clsx(
      styles.sideBarItem.default,
      styles.link.default,
      active && `${styles.link.active} ${styles.sideBarItem.active}`
    )}
    to={to}
    data-test={dataTest}
    onClick={onClick}
    disabled={isLoading}
    data-isloading={isLoading.toString().toLowerCase()}
  >
    {!isLoading && (
      <>
        {icon}
        {name}
      </>
    )}
    {isLoading && <Skeleton containerClassName={'basis-full'} />}
  </Link>
);
