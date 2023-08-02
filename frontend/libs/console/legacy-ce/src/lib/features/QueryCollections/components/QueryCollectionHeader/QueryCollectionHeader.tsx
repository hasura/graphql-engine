import React, { useState } from 'react';
import { FaPlusCircle } from 'react-icons/fa';

import { QueryCollectionEntry } from '../../../../metadata/types';
import { Button } from '../../../../new-components/Button';

import { QueryCollectionRenameDialog } from './QueryCollectionRenameDialog';
import { QueryCollectionHeaderMenu } from './QueryCollectionHeaderMenu';
import { QueryCollectionOperationAdd } from '../QueryCollectionOperationDialog/QueryCollectionOperationAdd';

interface QueryCollectionHeaderProps {
  queryCollection: QueryCollectionEntry;
  onDelete: (name: string) => void;
  onRename: (name: string, newName: string) => void;
}
export const QueryCollectionHeader: React.FC<
  QueryCollectionHeaderProps
> = props => {
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
      <div className="flex items-center mb-sm">
        <div>
          <h1 className="text-xl font-semibold">{queryCollection.name}</h1>
          <p className="text-muted m-0">
            Add queries to the collection to create a safe list of operations
            which can be run against your GraphQL API.
          </p>
        </div>
        <div className="relative ml-auto mr-sm">
          <QueryCollectionHeaderMenu
            queryCollection={queryCollection}
            onDelete={onDelete}
            onRename={onRename}
            setIsRenameModalOpen={setIsRenameModalOpen}
          />
        </div>
        <Button
          mode="primary"
          icon={<FaPlusCircle />}
          onClick={() => setIsAddModalOpen(true)}
        >
          Add Operation
        </Button>
      </div>
    </>
  );
};
