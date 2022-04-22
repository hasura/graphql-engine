import React from 'react';
import { Controller, useFormContext } from 'react-hook-form';
import { ListMap } from '@/new-components/ListMap';
import { DatabaseSelector } from '@/features/Data';
import { useTableColumns } from '@/features/SqlQueries';
import {
  LinkBlockHorizontal,
  LinkBlockVertical,
} from '@/new-components/LinkBlock';
import { Schema } from './schema';

export const FormElements = () => {
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
      <div className="grid grid-cols-12">
        <div className="col-span-5">
          <Controller
            control={control}
            name="source"
            render={({ field: { onChange, value }, formState: { errors } }) => (
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
            render={({ field: { onChange, value }, formState: { errors } }) => (
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

      <Controller
        control={control}
        name="mapping"
        render={({ field: { onChange, value } }) => (
          <ListMap
            onChange={onChange}
            fromLabel="Source Column"
            toLabel="Reference Column"
            maps={value}
            fromOptions={
              sourceColumnData
                ? sourceColumnData?.slice(1).map((x: string[]) => x[3])
                : []
            }
            toOptions={
              referenceColumnData
                ? referenceColumnData?.slice(1).map((x: string[]) => x[3])
                : []
            }
            name="mapping"
          />
        )}
      />
    </>
  );
};
