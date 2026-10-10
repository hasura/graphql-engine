import { useCallback } from 'react';
import { Button, Dialog, useConsoleForm } from '@hasura/shared/ui';

import {
  NativeQueryRelationshipFormSchema,
  nativeQueryRelationshipValidationSchema,
} from '../schema';
import { TrackNativeQueryRelationshipForm } from './TrackNativeQueryRelationshipForm';
import { MetadataWrapper } from '../../../components';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import { Flex } from '@radix-ui/themes';

export type NativeQueryRelationshipWidgetProps = {
  fromNativeQuery: string;
  dataSourceName: string;
  defaultValues?: NativeQueryRelationshipFormSchema;
  onCancel?: () => void;
  onSubmit?: (data: NativeQueryRelationshipFormSchema) => void;
  mode: 'create' | 'edit';
  asDialog?: boolean;
  isLoading?: boolean;
};

export const NativeQueryRelationshipWidget = ({
  fromNativeQuery,
  dataSourceName,
  asDialog,
  ...props
}: NativeQueryRelationshipWidgetProps) => {
  const isEditMode = props.mode === 'edit';

  const {
    Form,
    methods: { watch, handleSubmit },
  } = useConsoleForm({
    schema: nativeQueryRelationshipValidationSchema,
    options: {
      defaultValues: props.defaultValues,
    },
  });

  const targetNativeQuery = watch();

  const metadataSelector = useCallback(
    (m) => {
      const source = MetadataSelectors.findMetadataSource(dataSourceName, m);

      if (!source)
        throw new Error(
          `Unabled to find source ${dataSourceName} for Native Query Relationships Widget`,
        );

      const data = {
        models: source.logical_models ?? [],
        queries: source.native_queries ?? [],
      };

      const fromQuery = data.queries.find(
        (q) => q.root_field_name === fromNativeQuery,
      );
      const targetQuery = data.queries.find(
        (q) => q.root_field_name === targetNativeQuery.toNativeQuery,
      );
      const fromModel = data.models.find(
        (model) => model.name === fromQuery?.returns,
      );

      const targetModel = data.models.find(
        (model) => model.name === targetQuery?.returns,
      );

      return {
        ...data,
        fromQuery,
        targetQuery,
        fromModel,
        targetModel,
      };
    },
    [dataSourceName, fromNativeQuery, targetNativeQuery.toNativeQuery],
  );

  const onSubmit = (data: NativeQueryRelationshipFormSchema) => {
    // add/remove relationship
    props?.onSubmit?.(data);
  };

  const body = () => {
    return (
      <MetadataWrapper
        selector={metadataSelector}
        loader="skeleton"
        loadingStyle="overlay"
        skeletonProps={{
          count: 8,
        }}
        render={({ data }) => (
          <div className="px-4">
            <Form onSubmit={!asDialog ? onSubmit : () => {}}>
              <TrackNativeQueryRelationshipForm
                fromNativeQuery={data.fromQuery?.root_field_name ?? ''}
                nativeQueryOptions={data.queries
                  .map((q) => q.root_field_name)
                  .filter((q) => q !== fromNativeQuery)}
                fromFieldOptions={
                  data.fromModel?.fields.map((f) => f.name) ?? []
                }
                toFieldOptions={
                  data.targetModel?.fields.map((f) => f.name) ?? []
                }
              />
              {!asDialog && (
                <Flex justify="end">
                  <Button type="submit" mode="primary">
                    {isEditMode ? 'Edit Relationship' : 'Add Relationship'}
                  </Button>
                </Flex>
              )}
            </Form>
          </div>
        )}
      />
    );
  };

  // for transparency, as of this comment, there are no !asDialog implementations of this component.
  // see ../useWidget.tsx for implemenation
  if (!asDialog) {
    return body();
  }

  return (
    <Dialog
      size="xl"
      description="Create object or arrary relationships between Native Queries."
      footer={{
        onSubmit: () => {
          handleSubmit(onSubmit)();
        },
        onClose: props.onCancel,
        isLoading: props.isLoading,
        callToDeny: 'Cancel',
        callToAction: 'Save',
        onSubmitAnalyticsName: 'actions-tab-generate-types-submit',
        onCancelAnalyticsName: 'actions-tab-generate-types-cancel',
      }}
      title={`${props.mode === 'create' ? 'Add' : 'Edit'} Relationship`}
      onClose={props.onCancel}
    >
      {body()}
    </Dialog>
  );
};
