import { Table } from '@/features/hasura-metadata-types';
import { Button } from '@/new-components/Button';
import { InputField, Select, SimpleForm } from '@/new-components/Form';
import React from 'react';
import { FaArrowRight } from 'react-icons/fa';
import { useManageLocalRelationship } from '../../hooks/useManageLocalRelationship';
import { LocalRelationship } from '../../types';
import { MapColumns } from '../common/ColumnMapping';
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
      fromSource: data.fromSource.value.dataSourceName,
      fromTable: data.fromSource.value.table,
      relationshipType: data.relationship_type,
      definition: {
        toTable: data.toSource.value.table,
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
    <SimpleForm
      schema={schema}
      options={{
        defaultValues: {
          relationship_type: 'Object',
          fromSource: {
            value: {
              dataSourceName,
              table,
            },
          },
          toSource: {
            value: {
              dataSourceName,
            },
          },
          columnMap: [{ from: '', to: '' }],
        },
      }}
      onSubmit={handleFormSubmit}
    >
      <div id="create-local-rel" className="mt-4 px-7">
        <InputField
          name="name"
          label="Relationship Name"
          placeholder="Name..."
          dataTest="local-db-to-db-rel-name"
        />

        <div>
          <div className="grid grid-cols-12">
            <div className="col-span-5">
              <TablePicker type="fromSource" disabled isCurrentSource />
            </div>

            <div className="col-span-2 flex relative items-center justify-center w-full py-2 mt-3 text-muted">
              <FaArrowRight />
            </div>

            <div className="col-span-5">
              <TablePicker type="toSource" filterDataSource={dataSourceName} />
            </div>
          </div>

          <div className="bg-white rounded-md shadow-sm border border-gray-300 mt-2 mb-4">
            <div className="p-3 text-slate-900 font-semibold text-lg border-b border-gray-300">
              Relationship Details
            </div>
            <div className="px-6 pt-4">
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
            </div>
            <MapColumns />
          </div>
        </div>
      </div>
      <div className="flex justify-end gap-2 sticky bottom-0 bg-slate-50 px-8 py-3 border-t border-gray-300 z-[100]">
        <Button onClick={onCancel}>Close</Button>
        <Button
          type="submit"
          mode="primary"
          isLoading={isLoading}
          loadingText="Creating"
        >
          Create Relationship
        </Button>
      </div>
    </SimpleForm>
  );
};
