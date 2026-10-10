import { EventTrigger, Table as HasuraTable } from '@hasura/shared/types';
import { Table, Text } from '@hasura/shared/ui';
import { getTableDisplayName } from '@hasura/shared/utils';
import { Flex } from '@radix-ui/themes';

type ETInfoProps = {
  source: string;
  table: HasuraTable;
  currentTrigger: EventTrigger;
};

const Info = ({ source, table, currentTrigger }: ETInfoProps) => (
  <Flex direction="column" className="mb-4 w-6/12" gap="2">
    <Table.Root variant="surface">
      <Table.Body>
        <Table.Row>
          <Table.ColumnHeaderCell>Trigger Name</Table.ColumnHeaderCell>
          <Table.Cell>{currentTrigger.name}</Table.Cell>
        </Table.Row>
        <Table.Row>
          <Table.ColumnHeaderCell>Table</Table.ColumnHeaderCell>
          <Table.Cell>{getTableDisplayName(table)}</Table.Cell>
        </Table.Row>
        <Table.Row>
          <Table.ColumnHeaderCell>Data Source</Table.ColumnHeaderCell>
          <Table.Cell>{source}</Table.Cell>
        </Table.Row>
      </Table.Body>
    </Table.Root>
    <div>
      <Text as="p" size="1" color="gray">
        *Remove this trigger and create a new one to replace these options.
      </Text>
    </div>
  </Flex>
);

export default Info;
