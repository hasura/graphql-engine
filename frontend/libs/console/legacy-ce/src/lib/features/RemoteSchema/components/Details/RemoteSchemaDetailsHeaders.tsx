import { CardedTable, Text } from '@hasura/shared/ui';
import { HeaderConfig } from '@hasura/shared/types';

interface RemoteSchemaDetailsHeadersProps {
  headers?: HeaderConfig[];
  title?: string;
}
export const RemoteSchemaDetailsHeaders = (
  props: RemoteSchemaDetailsHeadersProps,
) => {
  const { headers, title = 'Headers' } = props;

  if (!headers) {
    return null;
  }

  const filteredHeaders = headers.filter((h) => !!h.name);

  return (
    <div className="mb-4">
      <Text as="div" weight="bold">
        {title}
      </Text>
      <CardedTable
        columns={['Name', 'Type', 'Value']}
        data={filteredHeaders.map((header) => {
          if ('value' in header) {
            return [header.name, 'Static', header.value];
          }
          if ('value_from_env' in header) {
            return [header.name, 'From env var', header.value_from_env];
          }
          return ['', '', ''];
        })}
      />
    </div>
  );
};
