import React from 'react';
import { FaStar, FaUser, FaXTwitter } from 'react-icons/fa6';
import { DropdownMenu, Text, ThemeToggleMenuItem } from '@hasura/shared/ui';
import { linkStyle } from './HeaderNavItem';
import { Flex } from '@radix-ui/themes';

type Props = {
  items?: React.ReactElement<any>[];
};

const UserMenu: React.FC<Props> = ({ items }) => {
  return (
    <DropdownMenu.Root
      items={[
        <ThemeToggleMenuItem key="theme-toggle" />,
        <DropdownMenu.Separator key="separator-theme" />,
        ...(items?.length
          ? [...items, <DropdownMenu.Separator key="separator-items" />]
          : []),
        <DropdownMenu.Item key="star">
          <Text asChild>
            <a
              href="https://github.com/hasura/graphql-engine"
              target="_blank"
              rel="noopener noreferrer"
            >
              <Flex align="center" gap="2">
                <FaStar />
                Star
              </Flex>
            </a>
          </Text>
        </DropdownMenu.Item>,
        <DropdownMenu.Item key="tweet">
          <Text asChild>
            <a
              href="https://twitter.com/intent/tweet?hashtags=graphql,postgres&text=Just%20deployed%20a%20GraphQL%20backend%20with%20@HasuraHQ!%20%E2%9D%A4%EF%B8%8F%20%F0%9F%9A%80%0Ahttps://github.com/hasura/graphql-engine%0A"
              target="_blank"
              rel="noopener noreferrer"
            >
              <Flex align="center" gap="2">
                <FaXTwitter />
                Tweet
              </Flex>
            </a>
          </Text>
        </DropdownMenu.Item>,
      ]}
    >
      <button type="button" aria-label="User menu" className={linkStyle}>
        <FaUser
          aria-hidden="true"
          className="fill-white cursor-pointer w-4 h-4"
        />
      </button>
    </DropdownMenu.Root>
  );
};

export default UserMenu;
