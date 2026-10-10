import { useMetadataMigrationMutation } from '../metadata';
import {
  NotImplementedError,
  QualifiedDataSource,
  Table,
} from '@hasura/shared/types';
import { getDriverPrefix } from '@hasura/metadata/helpers';

export type DropComputedFieldProps = {
  table: Table;
  name: string;
  source: QualifiedDataSource;
  cascade?: boolean;
  metadata_version?: number;
};

export function useDropComputedField() {
  return useMetadataMigrationMutation(
    async ({ source, metadata_version, ...args }: DropComputedFieldProps) => {
      const prefix = getDriverPrefix(source.kind);
      if (prefix !== 'pg' && prefix !== 'bigquery') {
        throw new NotImplementedError(
          `Driver ${source.kind} does not support computed fields`,
        );
      }

      return {
        type: `${prefix}_drop_computed_field`,
        args: {
          ...args,
          source: source.name,
        },
        metadata_version,
      };
    },
  );
}
