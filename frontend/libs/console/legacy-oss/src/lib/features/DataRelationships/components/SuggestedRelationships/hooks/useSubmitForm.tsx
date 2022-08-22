import { z } from 'zod';

import { useFireNotification } from '@/new-components/Notifications';
import { DataTarget } from '@/features/Datasources';
import {
  allowedMetadataTypes,
  TableRelationship,
  useMetadataMigration,
} from '@/features/MetadataAPI';

export const schema = z.object({
  relationshipName: z
    .string()
    .min(3, 'Relationship name must be at least 3 characters long'),
});

export type Schema = z.infer<typeof schema>;

export interface SuggestedRelationshipProps {
  target: DataTarget;
}

interface UseSubmitArgs {
  relationshipName: string;
  target: DataTarget;
  relationship: Omit<TableRelationship, 'name' | 'comment'>;
}

export const useSubmit = () => {
  const { fireNotification } = useFireNotification();
  const mutation = useMetadataMigration({
    onSuccess: () => {
      fireNotification({
        title: 'Success!',
        message: 'Relationship added successfully',
        type: 'success',
      });
    },
    onError: () => {
      fireNotification({
        title: 'Error',
        message: 'Error while adding the relationship',
        type: 'error',
      });
    },
  });

  const submit = async ({
    relationshipName,
    target,
    relationship,
  }: UseSubmitArgs) => {
    if (relationship.type === 'object') {
      const query = {
        type: 'pg_create_object_relationship' as allowedMetadataTypes,
        args: {
          table: target.table,
          name: relationshipName,
          source: target.database,
          using: {
            foreign_key_constraint_on: relationship.from.column,
          },
        },
      };

      return mutation.mutate({ query });
    }

    const query = {
      type: 'pg_create_array_relationship' as allowedMetadataTypes,
      args: {
        table: target.table,
        name: relationshipName,
        source: target.database,
        using: {
          foreign_key_constraint_on: {
            table: relationship.to.table,
            columns: relationship.to.column,
          },
        },
      },
    };
    return mutation.mutate({ query });
  };

  return { submit, ...mutation };
};
