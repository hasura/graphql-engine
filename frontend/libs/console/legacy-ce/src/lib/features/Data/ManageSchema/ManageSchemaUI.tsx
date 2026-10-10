import { SlOptionsVertical } from 'react-icons/sl';
import {
  Badge,
  Button,
  CardedTable,
  DisplayToastErrorMessage,
  DropdownMenu,
  hasuraToast,
  IconButton,
  SearchInput,
  useDestructiveConfirm,
  useHasuraAlert,
} from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';
import { Source } from '@hasura/shared/types';
import { usePaginatedSearchableList } from '../TrackResources/hooks';
import { PageSizeDropdown } from '../TrackResources/components/PageSizeDropdown';
import {
  useCreateDatabaseSchema,
  useDeleteDatabaseSchema,
} from '@hasura/metadata/data-source';
import { dataRoutes, getErrorContent } from '@hasura/shared/utils';
import { MdOutlineCreateNewFolder } from 'react-icons/md';
import { useNavigate } from 'react-router';

type ManageSchemaUIProps = {
  source: Source;
  schemaList: string[];
  refetch: () => void;
};

const ManageSchemaUI = ({
  source,
  schemaList,
  refetch,
}: ManageSchemaUIProps) => {
  const navigate = useNavigate();
  const { mutateAsync: createSchema } = useCreateDatabaseSchema();
  const { mutateAsync: deleteSchema, isPending: isDeleting } =
    useDeleteDatabaseSchema();

  const { hasuraPrompt } = useHasuraAlert();
  const destructiveConfirm = useDestructiveConfirm();

  const searchFn = (query: string, item: { id: string }) => {
    return item.id.toLowerCase().includes(query.toLowerCase());
  };

  const listProps = usePaginatedSearchableList({
    data: schemaList.map((item) => ({
      id: item,
    })),
    filterFn: searchFn,
  });

  const handleDropSchema = (name: string) => {
    destructiveConfirm({
      resourceName: name,
      resourceType: 'Schema',
      destroyTerm: 'delete',
      onConfirm: async () => {
        return deleteSchema({ schemaName: name, source })
          .then(() => {
            hasuraToast({
              title: 'Success!',
              message: 'Schema deleted successfully!',
              type: 'success',
            });

            return true;
          })
          .catch((err) => {
            hasuraToast({
              type: 'error',
              title: 'Dropping schema failed',
              children: (
                <DisplayToastErrorMessage message={getErrorContent(err)} />
              ),
            });

            return false;
          });
      },
    });
  };

  const handleCreateSchema = () => {
    hasuraPrompt({
      message: 'Type a name for your new schema:',
      title: 'Create Schema',
      sanitizeGraphQL: true,
      confirmText: 'Create',
      onCloseAsync: async (result) => {
        if (!result.confirmed)
          return {
            withSuccess: false,
          };

        return createSchema({
          schemaName: result.promptValue,
          source,
        })
          .then(() => {
            hasuraToast({
              title: 'Success!',
              message: 'Schema created successfully!',
              type: 'success',
            });

            return { withSuccess: true, successText: 'Schema Created' };
          })
          .catch((err) => {
            hasuraToast({
              title: 'Error while creating the schema',
              type: 'error',
              children: (
                <DisplayToastErrorMessage message={getErrorContent(err)} />
              ),
            });
            return { withSuccess: false };
          });
      },
    });
  };

  return (
    <div className="space-y-4">
      <Flex align="center" justify="between">
        <Button
          mode="default"
          leftIcon={MdOutlineCreateNewFolder}
          onClick={handleCreateSchema}
        >
          Create Schema
        </Button>
        {/* Search Input */}
        <Flex gap="2" align="center">
          <SearchInput onSearch={listProps.handleSearch} />
          {listProps.searchIsActive ? (
            <Badge>{listProps.filteredData.length} results found</Badge>
          ) : null}
        </Flex>
        <PageSizeDropdown {...listProps} />
      </Flex>
      <CardedTable
        columns={[
          'Schema',
          <Flex key="schema-actions" justify="end" align="center">
            <DropdownMenu.Root
              items={[
                <DropdownMenu.Item key="refresh" onSelect={() => refetch()}>
                  Refresh
                </DropdownMenu.Item>,
              ]}
            >
              <IconButton variant="ghost" radius="full">
                <SlOptionsVertical />
              </IconButton>
            </DropdownMenu.Root>
          </Flex>,
        ]}
        data={listProps.paginatedData.map((item) => [
          item.id,
          <Flex key={item.id} gap="2" justify="end">
            <Button
              mode="primary"
              size="1"
              onClick={() =>
                navigate(dataRoutes.addTable(source.name!, item.id))
              }
            >
              Create Table
            </Button>
            <Button
              size="sm"
              mode="destructive"
              onClick={() => {
                handleDropSchema(item.id);
              }}
              loadingText="Please wait"
              disabled={isDeleting}
            >
              Delete
            </Button>
          </Flex>,
        ])}
      />
    </div>
  );
};

export default ManageSchemaUI;
