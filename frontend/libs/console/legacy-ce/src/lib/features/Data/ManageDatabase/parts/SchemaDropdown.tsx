import { Flex } from '@radix-ui/themes';
import { BsDatabaseFillGear } from 'react-icons/bs';
import { MdAdd, MdEditNote } from 'react-icons/md';
import { DropdownMenu, IconButton } from '@hasura/shared/ui';
import { QualifiedDataSource } from '@hasura/shared/types';
import { dataRoutes } from '@hasura/shared/utils';
import { useNavigate } from 'react-router';

type SchemaDropdownProps = {
  source: QualifiedDataSource;
};

export function SchemaDropdown({ source }: SchemaDropdownProps) {
  const push = useNavigate();

  const renderButton = (disabled: boolean) => (
    <IconButton mode="default" disabled={disabled}>
      <BsDatabaseFillGear />
    </IconButton>
  );

  return (
    <DropdownMenu.Root
      items={[
        <>
          <DropdownMenu.Label>Connection</DropdownMenu.Label>
          <DropdownMenu.Item
            onClick={() => push(dataRoutes.editDatabase(source))}
          >
            <Flex gap="3" align="center">
              <MdEditNote />
              Edit Connection
            </Flex>
          </DropdownMenu.Item>
          <DropdownMenu.Label>Table</DropdownMenu.Label>
          <DropdownMenu.Item
            onClick={() => push(dataRoutes.addTable(source.name))}
          >
            <Flex gap="3" align="center">
              <MdAdd />
              New Table
            </Flex>
          </DropdownMenu.Item>
        </>,
      ]}
    >
      {renderButton(false)}
    </DropdownMenu.Root>
  );
}
