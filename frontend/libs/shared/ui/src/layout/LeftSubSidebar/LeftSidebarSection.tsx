import { useState, ReactElement } from 'react';
import { Input, Text } from '../../components';
import { FaSearch } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import { RelativeLink } from '../../components/link';

interface LeftSidebarItem {
  name: string;
}

interface LeftSidebarSectionProps<
  T extends LeftSidebarItem = LeftSidebarItem,
> extends React.ComponentProps<'div'> {
  items: T[];
  currentItem?: T;
  getServiceEntityLink: (s: string) => string;
  service: string;
  sidebarIcon?: ReactElement<any>;
}

export const useLeftSidebarSection = <
  T extends LeftSidebarItem = LeftSidebarItem,
>({
  items = [],
  currentItem,
  service,
  sidebarIcon,
  getServiceEntityLink,
}: LeftSidebarSectionProps<T>) => {
  const [searchText, setSearchText] = useState('');

  const handleSearch = (e: React.ChangeEvent<HTMLInputElement>) =>
    setSearchText(e.target.value);

  const getSearchInput = () => {
    return (
      <Input
        type="text"
        onChange={handleSearch}
        placeholder={`search ${service}`}
        data-test={`search-${service}`}
        icon={FaSearch}
        full
      />
    );
  };

  const itemList: T[] = [];

  if (searchText) {
    const secondaryResults: T[] = [];
    items.forEach((a) => {
      const lowerName = a.name.toLowerCase();
      const lowerText = searchText.toLowerCase();

      if (lowerName.startsWith(lowerText)) {
        itemList.push(a);
      } else if (lowerName.includes(lowerText)) {
        secondaryResults.push(a);
      }
    });
    itemList.push(...secondaryResults);
  } else {
    itemList.push(...items);
  }

  const getChildList = () => {
    if (itemList.length === 0) {
      return (
        <Text className="italic" as="div" data-test="sidebar-no-services">
          No {service} available
        </Text>
      );
    }

    return itemList.map((a) => {
      return (
        <RelativeLink
          color={currentItem?.name === a.name ? 'indigo' : 'gray'}
          weight={currentItem?.name === a.name ? 'medium' : 'regular'}
          to={getServiceEntityLink(a.name)}
          key={a.name}
          data-test={`action-sidebar-links-${a.name}`}
          className="block min-w-0"
        >
          <Flex align="center" gap="2" className="min-w-0">
            <span className="flex shrink-0">{sidebarIcon}</span>
            <span className="truncate" title={a.name}>
              {a.name}
            </span>
          </Flex>
        </RelativeLink>
      );
    });
  };

  return {
    getChildList,
    getSearchInput,
    searchText,
    count: itemList.length,
    items: itemList,
  };
};
