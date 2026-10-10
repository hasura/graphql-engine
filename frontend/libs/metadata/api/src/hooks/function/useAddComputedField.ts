import { useMetadataMigrationMutation } from '../metadata';
import {
  ComputedField,
  NotImplementedError,
  QualifiedDataSource,
  Table,
} from '@hasura/shared/types';
import { getDriverPrefix } from '@hasura/metadata/helpers';

export type AddComputedFieldProps = ComputedField & {
  table: Table;
  source: QualifiedDataSource;
  metadata_version?: string;
};

export function useAddComputedField() {
  return useMetadataMigrationMutation(
    async ({ source, metadata_version, ...args }: AddComputedFieldProps) => {
      const prefix = getDriverPrefix(source.kind);
      if (prefix !== 'pg' && prefix !== 'bigquery') {
        throw new NotImplementedError(
          `Driver ${source.kind} does not support computed fields`,
        );
      }

      return {
        type: `${prefix}_add_computed_field`,
        args: {
          ...args,
          source: source.name,
        },
        metadata_version,
      };
    },
  );
}
