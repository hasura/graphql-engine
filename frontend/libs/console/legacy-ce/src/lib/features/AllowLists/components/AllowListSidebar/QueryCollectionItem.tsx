import { RelativeLink } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';
import React from 'react';
import { FaFolder, FaFolderOpen } from 'react-icons/fa';
import { To } from 'react-router';

interface QueryCollectionItemProps {
  className?: string;
  name: string;
  selected: boolean;
  to: To;
  onClick?: React.MouseEventHandler<HTMLAnchorElement>;
}

export const QueryCollectionItem: React.FC<QueryCollectionItemProps> = (
  props,
) => {
  const { name, selected, className, ...rest } = props;
  const Icon = selected ? FaFolderOpen : FaFolder;

  return (
    <RelativeLink
      className={className}
      underline="none"
      color={selected ? 'indigo' : 'gray'}
      {...rest}
    >
      <Flex align="center" gap="2" className="p-2">
        <Icon />
        {name}
      </Flex>
    </RelativeLink>
  );
};
