import React from 'react';
import { Flex } from '@radix-ui/themes';
import { Button, Text } from '@hasura/shared/ui';
import { FaFolderPlus } from 'react-icons/fa';
import { QueryCollectionCreateDialog } from './QueryCollectionCreateDialog';
import { AllowListStatus } from './AllowListStatus';

interface AllowListSidebarHeaderProps {
  onQueryCollectionCreate?: (name: string) => void;
}

export const AllowListSidebarHeader = (props: AllowListSidebarHeaderProps) => {
  const { onQueryCollectionCreate } = props;
  const [isCreateModalOpen, setIsCreateModalOpen] = React.useState(false);
  return (
    <div className="pb-4">
      {isCreateModalOpen && (
        <QueryCollectionCreateDialog
          onCreate={(name) => {
            if (onQueryCollectionCreate) {
              onQueryCollectionCreate(name);
            }
          }}
          onClose={() => setIsCreateModalOpen(false)}
        />
      )}
      <Flex direction="column" className="2xl:flex-row">
        <Flex align="center" gap="2">
          <Text
            weight="bold"
            className="uppercase tracking-wider whitespace-nowrap"
          >
            Allow List
          </Text>
          <AllowListStatus />
        </Flex>
        {onQueryCollectionCreate && (
          <div className="mt-2 2xl:mt-0 2xl:ml-auto">
            <Button
              leftIcon={FaFolderPlus}
              size="1"
              onClick={() => setIsCreateModalOpen(true)}
            >
              Add Collection
            </Button>
          </div>
        )}
      </Flex>
    </div>
  );
};
