import { Table } from '@/features/hasura-metadata-types';
import { Button } from '@/new-components/Button';
import { InputField, Select, UpdatedForm } from '@/new-components/Form';
import React from 'react';
import { useManageLocalRelationship } from '../../hooks/useManageLocalRelationship';
import { LocalRelationship } from '../../types';
import { MapColumns } from '../common/ColumnMapping';
import { LinkBlockHorizontal } from '../common/LinkBlockHorizontal';
import { LinkBlockVertical } from '../common/LinkBlockVertical';
import { TablePicker } from '../common/TablePicker';
import { Schema, schema } from './schema';

interface WidgetProps {
  dataSourceName: string;
  table: Table;
  onCancel: () => void;
  onSuccess: () => void;
  onError: (err: Error) => void;
}

export const Widget = (props: WidgetProps) => {
  const { table, dataSourceName, onCancel, onSuccess, onError } = props;

  const { createRelationship, isLoading } = useManageLocalRelationship({
    dataSourceName,
    onSuccess,
    onError,
  });

  const handleFormSubmit = (data: Schema) => {
    // Form the local relationship object from the FormInputs
    const localRelationship: LocalRelationship = {
      name: data.name,
      type: 'localRelationship',
      fromSource: data.fromSource.dataSourceName,
      fromTable: data.fromSource.table,
      relationshipType: data.relationship_type,
      definition: {
        toTable: data.toSource.table,
        mapping: (data.columnMap ?? []).reduce(
          (acc, entry) => ({ ...acc, [entry.from]: entry.to }),
          {}
        ),
      },
    };

    // Call the hook method to create the same on the metadata
    createRelationship(localRelationship);
  };

  return (
    <UpdatedForm
      schema={schema}
      options={{
        defaultValues: {
          relationship_type: 'Object',
          fromSource: {
            dataSourceName,
            table,
          },
          toSource: {
            dataSourceName,
          },
          columnMap: [{ from: '', to: '' }],
        },
      }}
      onSubmit={handleFormSubmit}
    >
      {() => (
        <div id="create-local-rel" className="mt-4">
          <InputField
            name="name"
            label="Name"
            placeholder="Relationship name"
            dataTest="local-db-to-db-rel-name"
          />

          <Select
            name="relationship_type"
            label="Relationship Type"
            dataTest="local-db-to-db-select-rel-type"
            placeholder="Select a relationship type..."
            options={[
              {
                label: 'Object Relationship',
                value: 'Object',
              },
              {
                label: 'Array Relationship',
                value: 'Array',
              },
            ]}
          />

          <div>
            <div className="grid grid-cols-12">
              <div className="col-span-5">
                <div className="rounded bg-gray-50 border border-gray-300 p-md gap-y-4 border-l-4 border-l-green-600">
                  <TablePicker
                    name="fromSource"
                    options={{
                      dataSource: { disabled: true },
                      table: { disabled: true },
                    }}
                  />
                </div>
              </div>

              <LinkBlockHorizontal />

              <div className="col-span-5">
                <div className="rounded bg-gray-50 border border-gray-300 p-md gap-y-4 border-l-4 border-l-indigo-600">
                  <TablePicker
                    name="toSource"
                    options={{
                      dataSource: { disabled: true },
                    }}
                  />
                </div>
              </div>
            </div>

            <LinkBlockVertical title="Columns Mapped To" />

            <MapColumns />

            <div className="flex justify-end gap-2 mb-md">
              <Button onClick={onCancel}>Close</Button>
              <Button
                type="submit"
                mode="primary"
                isLoading={isLoading}
                loadingText="Creating"
              >
                Create
              </Button>
            </div>
          </div>
        </div>
      )}
    </UpdatedForm>
  );
};
