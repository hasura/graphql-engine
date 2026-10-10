import React from 'react';
import { FaLink, FaTimes } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import { IconButton, Tabs as RadixTabs, TabsProps } from '@hasura/shared/ui';

interface TableTabViewProps extends TabsProps {
  activeTab: string;
  onTabClick: (value: string) => void;
  onTabClose: (value: string) => void;
}

export const TableTabView: React.FC<TableTabViewProps> = (props) => {
  const { items, activeTab, onTabClick, onTabClose, ...rest } = props;
  return (
    <RadixTabs
      {...rest}
      onValueChange={onTabClick}
      defaultValue={items[0]?.value}
      value={activeTab}
      items={items.map(({ value, label, content }, index) => ({
        label:
          index !== 0 ? (
            <Flex align="center" gap="2">
              {label}
              <IconButton
                variant="ghost"
                radius="full"
                onClick={() => onTabClose(value)}
              >
                <FaTimes />
              </IconButton>
            </Flex>
          ) : (
            label
          ),
        value,
        content,
        icon: index !== 0 ? <FaLink /> : undefined,
      }))}
    />
  );
};
