import React, { ReactElement, useEffect, useMemo, useState } from 'react';
import { Link } from 'react-router';
import { FaDatabase } from 'react-icons/fa';
import { Em, Flex, Text } from '@radix-ui/themes';

type CollapsibleItemsProps = {
  source: string;
  currentItem?: { name: string; source: string };
  getServiceEntityLink: (v: string) => string;
  items: { name: string; source: string }[];
  icon: ReactElement<any>;
  defaultOpen?: boolean;
};
const CollapsibleItems: React.FC<CollapsibleItemsProps> = ({
  currentItem,
  source,
  getServiceEntityLink,
  items,
  icon,
  defaultOpen = false,
}) => {
  const [isOpen, setIsOpen] = useState(defaultOpen);

  useEffect(() => {
    setIsOpen(defaultOpen);
  }, [defaultOpen]);
  return (
    <div className="max-h-[360px] overflow-y-auto">
      <Flex
        onClick={() => setIsOpen((prev) => !prev)}
        onKeyDown={() => setIsOpen((prev) => !prev)}
        role="button"
        align="center"
        className="cursor-pointer"
        gap="2"
      >
        <FaDatabase /> <Text weight="medium">{source}</Text>
      </Flex>
      {isOpen ? (
        <Flex direction="column" gap="1" className="pl-6">
          {items.map(({ name }) => (
            <Link key={name} to={getServiceEntityLink(name)} data-test={name}>
              <Text
                size="2"
                color={
                  currentItem && currentItem.name === name ? 'indigo' : 'gray'
                }
                data-test={`action-sidebar-links-${name}`}
              >
                <Flex align="center" gap="2">
                  {icon}
                  {name}
                </Flex>
              </Text>
            </Link>
          ))}
        </Flex>
      ) : null}
    </div>
  );
};

export type SourceItem = {
  name: string;
  source: string;
};

type TreeViewProps = {
  service: string;
  items: SourceItem[];
  currentItem?: SourceItem;
  getServiceEntityLink: (name: string) => string;
  searchText?: string;
  icon: ReactElement<any>;
};

export const TreeView: React.FC<TreeViewProps> = ({
  items,
  service,
  currentItem,
  searchText,
  ...rest
}) => {
  const itemsBySource = useMemo(() => {
    return items.reduce(
      (acc, item) => {
        return {
          ...acc,
          [item.source]: acc[item.source]
            ? [...acc[item.source], item]
            : [item],
        };
      },
      {} as Record<string, { name: string; source: string }[]>,
    );
  }, [items]);

  if (items.length === 0) {
    return (
      <Text as="div" data-test="sidebar-no-services">
        <Em>No {service} available</Em>
      </Text>
    );
  }

  return (
    <div>
      {Object.keys(itemsBySource).map((source) => (
        <CollapsibleItems
          {...rest}
          key={source}
          source={source}
          items={itemsBySource[source]}
          currentItem={currentItem}
          defaultOpen={
            source === currentItem?.source ||
            (Boolean(searchText) && itemsBySource[source].length > 0)
          }
        />
      ))}
    </div>
  );
};
