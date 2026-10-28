import { Code, Flex, Text } from '@radix-ui/themes';
import { FaKey } from 'react-icons/fa';
import { Badge, DataList, Dialog, JsonCodeBlock } from '@hasura/shared/ui';
import { columnDataType, TableColumn } from '@hasura/metadata/data-source';
import { isJsonString } from '@hasura/shared/utils';

interface RowDialogProps {
  row: Record<string, unknown>;
  onClose: () => void;
  columns: TableColumn[];
}

const NullValue = () => (
  <Text color="gray" className="italic">
    NULL
  </Text>
);

const isPlainValue = (value: unknown) =>
  value === null || typeof value !== 'object';

const ArrayValue = ({ items }: { items: unknown[] }) => {
  if (!items.length)
    return (
      <Text color="gray" className="italic">
        Empty array
      </Text>
    );

  // nested arrays or objects read better as JSON
  if (!items.every(isPlainValue)) return <JsonCodeBlock value={items} />;

  return (
    <Flex gap="1" wrap="wrap">
      {items.map((item, index) => (
        <Code
          key={index}
          variant="soft"
          color={item === null ? 'gray' : undefined}
        >
          {item === null ? 'NULL' : String(item)}
        </Code>
      ))}
    </Flex>
  );
};

/** Renders a column value with a component suited to its data type. */
const RowValue = ({
  value,
  column,
}: {
  value: unknown;
  column?: TableColumn;
}) => {
  if (value === null || value === undefined) return <NullValue />;

  switch (column?.consoleDataType) {
    case 'boolean':
      return <Badge color={value ? 'green' : 'gray'}>{String(value)}</Badge>;

    case 'json':
      return (
        <JsonCodeBlock
          scrollable
          className="w-full"
          value={
            typeof value === 'string' && isJsonString(value)
              ? JSON.parse(value)
              : value
          }
        />
      );

    case 'array':
      if (Array.isArray(value)) return <ArrayValue items={value} />;
      break;

    case 'integer':
    case 'number':
    case 'float':
    case 'uuid':
      return <Code variant="ghost">{String(value)}</Code>;
  }

  if (Array.isArray(value)) return <ArrayValue items={value} />;
  if (typeof value === 'object') return <JsonCodeBlock value={value} />;

  return (
    <Text className="whitespace-pre-wrap break-words">{String(value)}</Text>
  );
};

/**
 * Primary keys first (in table definition order), then the other columns of
 * the row by name, like the rows grid.
 */
const getRowColumns = (
  row: Record<string, unknown>,
  columns: TableColumn[],
) => {
  const primaryKeys = columns.filter(
    (column) => column.isPrimaryKey && column.name in row,
  );
  const others = Object.keys(row)
    .filter((key) => !primaryKeys.some((column) => column.name === key))
    .sort((a, b) => a.localeCompare(b))
    .map(
      (name) =>
        columns.find((column) => column.name === name) ??
        ({ name } as TableColumn),
    );

  return [...primaryKeys, ...others];
};

/** A read-only view of a table row. */
export const RowDialog = ({ onClose, row, columns }: RowDialogProps) => (
  <Dialog title="Table Row" onClose={onClose} size="xxxl">
    <DataList
      orientation={{ initial: 'vertical', sm: 'horizontal' }}
      size="2"
      items={getRowColumns(row, columns).map((column) => ({
        label: (
          <Flex direction="column" gap="1">
            <Flex align="center" gap="1">
              {column.isPrimaryKey && (
                <FaKey aria-label="Primary key" className="shrink-0" />
              )}
              <Text weight="medium" className="break-all">
                {column.name}
              </Text>
            </Flex>
            {column.dataType && (
              <Text size="1" color="gray">
                {columnDataType(column.dataType)}
              </Text>
            )}
          </Flex>
        ),
        value: (
          <div className="min-w-0" data-testid={`row-value-${column.name}`}>
            <RowValue value={row[column.name]} column={column} />
          </div>
        ),
      }))}
    />
  </Dialog>
);
