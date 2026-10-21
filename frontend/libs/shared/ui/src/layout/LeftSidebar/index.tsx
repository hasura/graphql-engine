import clsx from 'clsx';
import { Separator } from '../../components';
import NavItem, { LeftSidebarNavItemProps } from './NavItem';

export type LeftSidebarProps = {
  className?: string;
  items: LeftSidebarNavItemProps[];
};

export const LeftSidebar = ({ items, className }: LeftSidebarProps) => {
  return (
    <div className={clsx('h-full overflow-y-scroll', className)}>
      {items.map((item, i) => {
        if (i === 0) {
          return <NavItem key={`item-${i}`} {...item} />;
        }

        return (
          <div key={`item-${i}`}>
            <Separator size="4" />
            <NavItem {...item} />
          </div>
        );
      })}
    </div>
  );
};
