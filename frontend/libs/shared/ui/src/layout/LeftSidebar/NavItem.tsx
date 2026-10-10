import { To } from 'react-router';
import { Button, Text } from '@radix-ui/themes';
import clsx from 'clsx';
import { RelativeLink } from '../../components/link';

export type LeftSidebarNavItemProps = {
  isActive: boolean;
  label: React.ReactNode;
  to: To;
  children?: React.ReactNode;
};

const NavItem = ({
  isActive,
  label,
  children,
  to,
}: LeftSidebarNavItemProps) => {
  return (
    <div role="presentation">
      <RelativeLink className="block" to={to}>
        <Button
          color={isActive ? 'indigo' : 'gray'}
          variant="soft"
          size="4"
          className={clsx({
            'w-full! justify-start! text-left!': true,
          })}
          radius="none"
        >
          <Text size="4" weight={isActive ? 'medium' : 'light'}>
            {label}
          </Text>
        </Button>
      </RelativeLink>
      {children}
    </div>
  );
};

export default NavItem;
