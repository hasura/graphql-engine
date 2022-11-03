import { Table } from '@/features/MetadataAPI';
import { Button } from '@/new-components/Button';
import { UpdatedForm } from '@/new-components/Form';
import React, { useMemo } from 'react';
import { schema, Schema } from './schema';
import { MapColumns } from './parts/MapColumns';
import { Name } from './parts/Name';
import { RelationshipType } from './parts/RelationshipType';
import { SourceTable } from './parts/SourceTable';
import { TargetTable } from './parts/TargetTable';
import { useCreateManualLocalRelationship } from './hooks/useCreateManualLocalRelationship';
import { LinkBlockHorizontal } from './parts/LinkBlockHorizontal';
import { LinkBlockVertical } from './parts/LinkBlockVertical';
import { Relationship } from '../DatabaseRelationshipsTable/types';

export type ManualLocalRelationshipWidget = {
  dataSourceName: string;
  table: Table;

  onSuccess: () => void;

  existingRelationship?: Relationship;
};

export const ManualLocalRelationshipWidget = (
  props: ManualLocalRelationshipWidget
) => {
  const { dataSourceName, table, onSuccess, existingRelationship } = props;

  const {
    createManualLocalRelationship,
    editManualLocalRelationship,
    isLoading: isSaving,
  } = useCreateManualLocalRelationship({ onSuccess });

  const defaultValues: Schema | null = useMemo(() => {
    if (!existingRelationship)
      return {
        name: '',
        relationship_type: 'object',
        source_name: dataSourceName,
        source_table: table,
        target_name: dataSourceName,
        column_mapping: [{}] as Schema['column_mapping'],
      };

    if (existingRelationship?.type !== 'toLocalTableManual') return null;

    const existingColumnMapping = Object.entries(
      existingRelationship.definition.using.manual_configuration.column_mapping
    ).map(([key, value]) => ({ from: key, to: value }));
    const existingTable = existingRelationship.mapping.to.table;

    return {
      name: existingRelationship.name ?? '',
      relationship_type:
        existingRelationship?.relationship_type === 'Array'
          ? 'array'
          : 'object',
      source_name: dataSourceName,
      source_table: table,
      target_name: dataSourceName,
      target_table: JSON.stringify(existingTable),
      column_mapping: existingColumnMapping.length ? existingColumnMapping : [],
    };
  }, [dataSourceName, existingRelationship, table]);

  const mode = existingRelationship ? 'edit' : 'create';

  if (!defaultValues) return null;

  return (
    <UpdatedForm
      schema={schema}
      onSubmit={values => {
        if (mode === 'edit') {
          editManualLocalRelationship({
            relationshipName: existingRelationship?.name ?? '',
            newName: values.name,
            fromSource: values.source_name,
            fromTable: values.source_table,
          });
        } else {
          createManualLocalRelationship({
            relationshipName: values.name,
            relationshipType: values.relationship_type,
            fromSource: values.source_name,
            fromTable: values.source_table,
            toTable: values.target_table,
            columnMapping: values.column_mapping,
          });
        }
      }}
      options={{
        defaultValues,
      }}
    >
      {() => (
        <>
          <div className="w-full sm:w-6/12 mb-md">
            <div className="mb-sm">
              <Name />
            </div>
          </div>

          {mode === 'create' && (
            <div>
              <div className="mb-sm w-1/2">
                <RelationshipType disabled={false} />
              </div>

              <div className="grid grid-cols-12">
                <div className="col-span-5">
                  <div className="rounded bg-gray-50 border border-gray-300 p-md gap-y-4 border-l-4 border-l-green-600">
                    <SourceTable />
                  </div>
                </div>

                <LinkBlockHorizontal />
                <div className="col-span-5">
                  <div className="rounded bg-gray-50 border border-gray-300 p-md gap-y-4 border-l-4 border-l-indigo-600">
                    <TargetTable disabled={false} />
                  </div>
                </div>
              </div>

              <LinkBlockVertical title="Columns Mapped To" />

              <MapColumns disabled={false} />
            </div>
          )}

          <Button
            mode="primary"
            type="submit"
            loadingText="Saving relationship"
            data-testid="add-local-db-relationship"
            isLoading={isSaving}
          >
            Save Relationship
          </Button>
        </>
      )}
    </UpdatedForm>
  );
};
