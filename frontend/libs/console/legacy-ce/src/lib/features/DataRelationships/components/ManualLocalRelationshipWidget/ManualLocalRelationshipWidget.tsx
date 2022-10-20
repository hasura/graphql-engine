import {
  ManualArrayRelationship,
  ManualObjectRelationship,
  Table,
} from '@/features/MetadataAPI';
import { Button } from '@/new-components/Button';
import { UpdatedForm } from '@/new-components/Form';
import React from 'react';
import { schema } from './schema';
import { MapColumns } from './parts/MapColumns';
import { Name } from './parts/Name';
import { RelationshipType } from './parts/RelationshipType';
import { SourceTable } from './parts/SourceTable';
import { TargetTable } from './parts/TargetTable';
import { useCreateManualLocalRelationship } from './hooks/useCreateManualLocalRelationship';
import { LinkBlockHorizontal } from './parts/LinkBlockHorizontal';
import { LinkBlockVertical } from './parts/LinkBlockVertical';

export type ManualLocalRelationshipWidget = {
  dataSourceName: string;
  table: Table;

  onSuccess: () => void;

  existingRelationship?: ManualArrayRelationship | ManualObjectRelationship;
};

export const ManualLocalRelationshipWidget = (
  props: ManualLocalRelationshipWidget
) => {
  // Get current relationship if any for this table
  const { dataSourceName, table, onSuccess } = props;

  const { createManualLocalRelationship, isLoading: isSaving } =
    useCreateManualLocalRelationship({ onSuccess });

  return (
    <UpdatedForm
      schema={schema}
      onSubmit={(values) => {
        createManualLocalRelationship({
          relationshipName: values.name,
          relationshipType: values.relationship_type,
          fromSource: values.source_name,
          fromTable: values.source_table,
          toTable: values.target_table,
          columnMapping: values.column_mapping,
        });
      }}
      options={{
        defaultValues: {
          name: '',
          relationship_type: 'object',
          source_name: dataSourceName,
          source_table: table,
          target_name: dataSourceName,
          column_mapping: [{}],
        },
      }}
    >
      {() => (
        <>
          <div className="w-full sm:w-6/12 mb-md">
            <div className="mb-sm">
              <Name />
            </div>

            <div className="mb-sm">
              <RelationshipType />
            </div>
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
                <TargetTable />
              </div>
            </div>
          </div>

          <LinkBlockVertical title="Columns Mapped To" />

          <MapColumns />

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
