import { To, useLocation } from 'react-router';
import { FaCheckCircle, FaTimesCircle, FaRegCircle } from 'react-icons/fa';
import { ComponentProps } from 'react';
import { Flex } from '@radix-ui/themes';
import { sendTelemetryEvent } from '../../../../telemetry';
import { Button, RelativeLink, Text } from '@hasura/shared/ui';

export type NavigationSidebarItem = {
  key: string;
  label: string;
  route: To;
  status?: 'loading' | 'enabled' | 'disabled' | 'error' | 'none';
  dataTestVal?: string;
};

export type NavigationSidebarSection = {
  key: string;
  label: string;
  items: NavigationSidebarItem[];
};

export interface NavigationSidebarProps extends ComponentProps<'div'> {
  sections: NavigationSidebarSection[];
}

export const NavigationSidebar = ({ sections }: NavigationSidebarProps) => {
  const location = useLocation();

  const renderIconProps = (item: NavigationSidebarItem) => {
    switch (item.status) {
      case 'enabled':
        return {
          rightIcon: FaCheckCircle,
          iconClassName: 'text-emerald-600',
        };
      case 'error':
        return {
          rightIcon: FaTimesCircle,
          iconClassName: 'text-red-600',
        };
      case 'disabled':
        return {
          rightIcon: FaRegCircle,
        };
      default:
        return null;
    }
  };

  return (
    <Flex direction="column" gap="4" className="w-full p-3">
      {sections.map((section) => (
        <Flex direction="column" gap="2" key={section.key}>
          <Text color="gray" weight="bold" className="uppercase">
            {section.label}
          </Text>
          {section.items.map((item) => {
            const textColor = location?.pathname?.includes(
              item.route.toString(),
            )
              ? 'indigo'
              : 'gray';
            return (
              <RelativeLink
                className="block w-full"
                key={item.key}
                to={item.route}
                data-test={item.dataTestVal}
                onClick={() => {
                  // Check if section.key is "schema-registry" and trigger telemetry event
                  if (item.key === 'schema-registry') {
                    sendTelemetryEvent({
                      type: 'CLICK_EVENT',
                      data: {
                        id: 'schema-registry-settings-btn',
                      },
                    });
                  }
                }}
              >
                <Button
                  {...renderIconProps(item)}
                  className="justify-between!"
                  color={'gray'}
                  variant="ghost"
                  size="2"
                  full
                >
                  <Text color={textColor}>{item.label}</Text>
                </Button>
              </RelativeLink>
            );
          })}
        </Flex>
      ))}
    </Flex>
  );
};
