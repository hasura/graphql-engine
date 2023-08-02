import React from 'react';
import { CardedTable } from '../../../../new-components/CardedTable';
import { ServerHeader } from '../../../hasura-metadata-types';

interface RemoteSchemaDetailsHeadersProps {
  headers?: ServerHeader[];
}
export const RemoteSchemaDetailsHeaders = (
  props: RemoteSchemaDetailsHeadersProps
) => {
  const { headers } = props;

  if (!headers) {
    return null;
  }

  const filteredHeaders = headers.filter(h => !!h.name);

  return (
    <div className="mb-md">
      <label className="block mb-xs font-semibold text-muted">Headers</label>
      <CardedTable
        columns={['Name', 'Type', 'Value']}
        data={filteredHeaders.map(header => {
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
