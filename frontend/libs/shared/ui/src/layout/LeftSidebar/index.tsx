import { Separator } from '../../components';
import NavItem, { LeftSidebarNavItemProps } from './NavItem';

export type LeftSidebarProps = {
  items: LeftSidebarNavItemProps[];
};

export const LeftSidebar = ({ items }: LeftSidebarProps) => {
  return (
    <div className="h-screen overflow-y-scroll">
      {items.flatMap((item, i) => {
        const result = [<NavItem key={`item-${i}`} {...item} />];

        if (i < items.length - 1) {
          result.push(<Separator key={`separator-${i}`} size="4" />);
        }

        return result;
      })}
    </div>
  );
};
