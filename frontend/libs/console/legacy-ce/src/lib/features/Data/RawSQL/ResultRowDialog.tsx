import { Text } from '@radix-ui/themes';
import { DataList, Dialog, JsonCodeBlock } from '@hasura/shared/ui';
import { isJsonString } from '@hasura/shared/utils';

interface ResultRowDialogProps {
  headers: string[];
  row: Array<string | null>;
  onClose: () => void;
}

// `run_sql` returns every value as text: JSON objects and arrays are shown
// formatted, anything else as is
const ResultValue = ({ value }: { value: string | null | undefined }) => {
  if (value === null || value === undefined)
    return (
      <Text color="gray" className="italic">
        NULL
      </Text>
    );

  if (isJsonString(value)) {
    const json = JSON.parse(value);
    if (json !== null && typeof json === 'object')
      return <JsonCodeBlock scrollable className="w-full" value={json} />;
  }

  return <Text className="whitespace-pre-wrap break-words">{value}</Text>;
};

/**
 * A read-only view of a row of an SQL result, with its columns in the order
 * of the result: unlike a table row, column names can repeat.
 */
export const ResultRowDialog = ({
  headers,
  row,
  onClose,
}: ResultRowDialogProps) => (
  <Dialog title="Result Row" onClose={onClose} size="xxxl">
    <DataList
      orientation={{ initial: 'vertical', sm: 'horizontal' }}
      size="2"
      items={headers.map((header, index) => ({
        label: (
          <Text weight="medium" className="break-all">
            {header}
          </Text>
        ),
        value: (
          <div className="min-w-0" data-testid={`result-value-${index}`}>
            <ResultValue value={row[index]} />
          </div>
        ),
      }))}
    />
  </Dialog>
);
