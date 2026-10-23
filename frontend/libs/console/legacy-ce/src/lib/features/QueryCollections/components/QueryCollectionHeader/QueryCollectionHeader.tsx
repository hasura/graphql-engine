import React, { useState } from 'react';
import { FaPlusCircle } from 'react-icons/fa';

import { Button, Text } from '@hasura/shared/ui';

import { QueryCollectionRenameDialog } from './QueryCollectionRenameDialog';
import { QueryCollectionHeaderMenu } from './QueryCollectionHeaderMenu';
import { QueryCollectionOperationAdd } from '../QueryCollectionOperationDialog/QueryCollectionOperationAdd';
import type { QueryCollection } from '@hasura/shared/types';
import { Flex, Heading } from '@radix-ui/themes';

interface QueryCollectionHeaderProps {
  queryCollection: QueryCollection;
  onDelete: (name: string) => void;
  onRename: (name: string, newName: string) => void;
}
export const QueryCollectionHeader: React.FC<QueryCollectionHeaderProps> = (
  props,
) => {
  const { queryCollection, onDelete, onRename } = props;

  const [isRenameModalOpen, setIsRenameModalOpen] = useState(false);

  const [isAddModalOpen, setIsAddModalOpen] = useState(false);

  return (
    <>
      {isRenameModalOpen && (
        <QueryCollectionRenameDialog
          currentName={queryCollection.name}
          onClose={() => {
            setIsRenameModalOpen(false);
          }}
          onRename={onRename}
        />
      )}
      {isAddModalOpen && (
        <QueryCollectionOperationAdd
          queryCollectionName={queryCollection.name}
          onClose={() => {
            setIsAddModalOpen(false);
          }}
        />
      )}
      <Flex align="center" className="mb-2">
        <div>
          <Heading size="4">{queryCollection.name}</Heading>
          <Text as="p" className="text-muted m-0">
            Add queries to the collection to create a safe list of operations
            which can be run against your GraphQL API.
          </Text>
        </div>
        <div className="relative ml-auto mr-2">
          <QueryCollectionHeaderMenu
            queryCollection={queryCollection}
            onDelete={onDelete}
            onRename={onRename}
            setIsRenameModalOpen={setIsRenameModalOpen}
          />
        </div>
        <Button
          mode="primary"
          leftIcon={FaPlusCircle}
          onClick={() => setIsAddModalOpen(true)}
        >
          Add Operation
        </Button>
      </Flex>
    </>
  );
};
