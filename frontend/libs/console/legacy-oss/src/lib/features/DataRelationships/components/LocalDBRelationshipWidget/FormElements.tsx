import React from 'react';
import { Controller, useFormContext } from 'react-hook-form';
import { InputField, Select } from '@/new-components/Form';
import { ListMap } from '@/new-components/ListMap';
import { DatabaseSelector } from '@/features/Data';
import { useTableColumns } from '@/features/SqlQueries';
import {
  LinkBlockHorizontal,
  LinkBlockVertical,
} from '@/new-components/LinkBlock';
import { FaCircle } from 'react-icons/fa';
import { Schema } from './schema';

interface FormElementsProps {
  existingRelationshipName: string;
}

export const FormElements = ({
  existingRelationshipName,
}: FormElementsProps) => {
  const { control, watch } = useFormContext<Schema>();
  const [source, destination] = watch(['source', 'destination']);

  const { data: sourceColumnData } = useTableColumns(source.database, {
    name: source.table,
    schema: source.schema ?? source.dataset ?? '',
  });

  const { data: referenceColumnData } = useTableColumns(destination.database, {
    name: destination.table,
    schema: destination.schema ?? destination.dataset ?? '',
  });

  return (
    <>
      <div className="w-full sm:w-6/12 mb-md">
        <div className="mb-md">
          <InputField
            name="relationshipName"
            label="Name"
            placeholder="Relationship name"
            dataTest="local-db-to-db-rel-name"
            disabled={!!existingRelationshipName}
          />
        </div>

        <div className="mb-md">
          <Select
            name="relationshipType"
            label="Type"
            dataTest="local-db-to-db-select-rel-type"
            placeholder="Select a relationship type..."
            options={[
              {
                label: 'Object Relationship',
                value: 'create_object_relationship',
              },
              {
                label: 'Array Relationship',
                value: 'create_array_relationship',
              },
            ]}
          />
        </div>
      </div>

      <>
        <div className="grid grid-cols-12">
          <div className="col-span-5">
            <Controller
              control={control}
              name="source"
              render={({
                field: { onChange, value },
                formState: { errors },
              }) => (
                <DatabaseSelector
                  value={value}
                  onChange={onChange}
                  name="source"
                  errors={errors}
                  className="border-l-4 border-l-green-600"
                  hiddenKeys={['database']}
                  disabledKeys={['schema', 'table', 'database']}
                  labels={{
                    database: 'Source Database',
                    schema: 'Source Schema',
                    dataset: 'Source Dataset',
                    table: 'Source Table',
                  }}
                />
              )}
            />
          </div>

          <LinkBlockHorizontal />

          <div className="col-span-5">
            <Controller
              control={control}
              name="destination"
              render={({
                field: { onChange, value },
                formState: { errors },
              }) => (
                <DatabaseSelector
                  value={value}
                  onChange={onChange}
                  name="destination"
                  errors={errors}
                  className="border-l-4 border-l-indigo-600"
                  hiddenKeys={['database']}
                  labels={{
                    database: 'Reference Database',
                    schema: 'Reference Schema',
                    dataset: 'Reference Dataset',
                    table: 'Reference Table',
                  }}
                />
              )}
            />
          </div>
        </div>

        <LinkBlockVertical title="Columns Mapped To" />

        <ListMap
          name="mapping"
          existingRelationshipName={existingRelationshipName}
          from={{
            label: 'Source Column',
            options: sourceColumnData
              ? sourceColumnData?.slice(1).map((x: string[]) => x[3])
              : [],
            placeholder: 'Select Source Column',
            icon: <FaCircle className="text-green-600" />,
          }}
          to={{
            type: 'array',
            label: 'Reference Column',
            options: referenceColumnData
              ? referenceColumnData?.slice(1).map((x: string[]) => x[3])
              : [],
            placeholder: 'Select Reference Column',
            icon: <FaCircle className="text-indigo-600" />,
          }}
        />
      </>
    </>
  );
};
