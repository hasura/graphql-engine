import React, { ReactElement, useMemo, useState } from 'react';
import { FaDatabase } from 'react-icons/fa';
import { Flex, Text } from '@radix-ui/themes';
import { RelativeLink } from '@hasura/shared/ui';

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
  const [prevDefaultOpen, setPrevDefaultOpen] = useState(defaultOpen);

  // Keep the open state in sync with the `defaultOpen` prop by adjusting state
  // during render instead of in an effect (avoids a cascading render):
  // https://react.dev/learn/you-might-not-need-an-effect#adjusting-some-state-when-a-prop-changes
  if (prevDefaultOpen !== defaultOpen) {
    setPrevDefaultOpen(defaultOpen);
    setIsOpen(defaultOpen);
  }
  return (
    <div className="max-h-90 overflow-y-auto">
      <Flex
        onClick={() => setIsOpen((prev) => !prev)}
        onKeyDown={() => setIsOpen((prev) => !prev)}
        role="button"
        align="center"
        className="cursor-pointer"
        gap="2"
      >
        <FaDatabase className="shrink-0" />{' '}
        <Text weight="medium" className="truncate" title={source}>
          {source}
        </Text>
      </Flex>
      {isOpen ? (
        <Flex direction="column" gap="1" className="pl-6">
          {items.map(({ name }) => (
            <RelativeLink
              key={name}
              to={getServiceEntityLink(name)}
              data-test={name}
              className="block min-w-0"
              size="2"
              color={
                currentItem && currentItem.name === name ? 'indigo' : 'gray'
              }
              weight={
                currentItem && currentItem.name === name ? 'medium' : 'regular'
              }
            >
              <Flex align="center" gap="2" className="min-w-0">
                <span className="flex shrink-0">{icon}</span>
                <span className="truncate" title={name}>
                  {name}
                </span>
              </Flex>
            </RelativeLink>
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
      <Text
        className="italic"
        align="center"
        as="div"
        data-test="sidebar-no-services"
      >
        No {service} available
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
