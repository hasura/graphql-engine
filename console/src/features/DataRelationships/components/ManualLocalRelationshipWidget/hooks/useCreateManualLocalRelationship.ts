import { exportMetadata } from '@/features/DataSource';
import { Table, useMetadataMigration } from '@/features/MetadataAPI';
import { useHttpClient } from '@/features/Network';
import { useFireNotification } from '@/new-components/Notifications';
import { useCallback } from 'react';
import { useQueryClient } from 'react-query';

type CreateManualLocalRelationshipPayload = {
  relationshipName: string;
  relationshipType: 'object' | 'array';
  fromTable: Table;
  fromSource: string;
  toTable: Table;
  columnMapping: { from: string; to: string }[];
};

type EditManualLocalRelationshipPayload = { newName: string } & Pick<
  CreateManualLocalRelationshipPayload,
  'relationshipName' | 'fromSource' | 'fromTable'
>;

export const useCreateManualLocalRelationship = (props: {
  onSuccess?: () => void;
}) => {
  const { mutate, ...rest } = useMetadataMigration();
  const httpClient = useHttpClient();
  const { fireNotification } = useFireNotification();
  const queryClient = useQueryClient();

  const createManualLocalRelationship = useCallback(
    async (values: CreateManualLocalRelationshipPayload) => {
      const { metadata, resource_version } = await exportMetadata({
        httpClient,
      });

      if (!metadata) throw Error('Unable to fetch metadata');

      const metadataSource = metadata.sources.find(
        s => s.name === values.fromSource
      );

      if (!metadataSource) throw Error('Unable to fetch metadata source');

      const driver = metadataSource.kind;

      const type =
        values.relationshipType === 'object'
          ? 'create_object_relationship'
          : 'create_array_relationship';

      mutate(
        {
          query: {
            resource_version,
            type: `${driver}_${type}`,
            args: {
              table: values.fromTable,
              source: values.fromSource,
              name: values.relationshipName,
              using: {
                manual_configuration: {
                  remote_table: values.toTable,
                  column_mapping: values.columnMapping.reduce(
                    (acc, val) => ({ ...acc, [val.from]: val.to }),
                    {}
                  ),
                },
              },
            },
          },
        },
        {
          onSuccess: () => {
            queryClient.refetchQueries([
              values.fromSource,
              'list_all_relationships',
            ]);

            props.onSuccess?.();

            fireNotification({
              type: 'success',
              title: 'Success!',
              message: 'A relationship was added to Hasura succesfull!',
            });
          },
          onError: err => {
            fireNotification({
              type: 'error',
              title: 'Failed to create the relationship!',
              message: err?.message,
            });
          },
        }
      );
    },
    [fireNotification, httpClient, mutate, props, queryClient]
  );

  const editManualLocalRelationship = useCallback(
    async (values: EditManualLocalRelationshipPayload) => {
      const { metadata, resource_version } = await exportMetadata({
        httpClient,
      });

      if (!metadata) throw Error('Unable to fetch metadata');

      const metadataSource = metadata.sources.find(
        s => s.name === values.fromSource
      );

      if (!metadataSource) throw Error('Unable to fetch metadata source');

      const driver = metadataSource.kind;

      const type = 'rename_relationship';

      mutate(
        {
          query: {
            resource_version,
            type: `${driver}_${type}`,
            args: {
              table: values.fromTable,
              source: values.fromSource,
              name: values.relationshipName,
              new_name: values.newName,
            },
          },
        },
        {
          onSuccess: () => {
            queryClient.refetchQueries([
              values.fromSource,
              'list_all_relationships',
            ]);

            props.onSuccess?.();

            fireNotification({
              type: 'success',
              title: 'Success!',
              message: 'A relationship renamed succesfully!',
            });
          },
          onError: err => {
            fireNotification({
              type: 'error',
              title: 'Failed to rename relationship',
              message: err?.message,
            });
          },
        }
      );
    },
    [fireNotification, httpClient, mutate, props, queryClient]
  );

  return {
    createManualLocalRelationship,
    editManualLocalRelationship,
    ...rest,
  };
};
