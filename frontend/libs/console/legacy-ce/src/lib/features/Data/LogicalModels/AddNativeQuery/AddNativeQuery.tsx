import React from 'react';
import { FaSave } from 'react-icons/fa';
import {
  useHasuraAlert,
  Button,
  useConsoleForm,
  hasuraToast,
  Separator,
} from '@hasura/shared/ui';
import { NativeQuery } from '@hasura/shared/types';
import { MetadataWrapper } from '../../components';
import { useTrackNativeQuery } from '../../hooks/useTrackNativeQuery';
import { Routes } from '../constants';
import { NativeQueryFormFields } from './components/NativeQueryDetailsForm';
import { schema } from './schema';
import { NativeQueryForm } from './types';
import { normalizeArguments, transformFormOutputToMetadata } from './utils';
import { useAllDriverCapabilities } from '@hasura/metadata/data-source';
import { Flex } from '@radix-ui/themes';
import { useNavigate } from 'react-router';

type AddNativeQueryProps = {
  editDetails?: {
    nativeQuery: NativeQuery;
    dataSourceName: string;
  };
  // this is purely for storybook
  defaultSql?: string;
};

export const AddNativeQuery = ({
  editDetails,
  defaultSql,
}: AddNativeQueryProps) => {
  const { mode, defaultFormValues } = useDefaultFormValues({
    editDetails,
    defaultSql,
  });

  const {
    Form,
    methods: { watch, setValue, formState },
  } = useConsoleForm({
    schema,
    options: {
      defaultValues: defaultFormValues,
    },
  });

  const formHasChanges = Object.keys(formState.dirtyFields).length > 0;

  const push = useNavigate();

  React.useEffect(() => {
    const subscription = watch((value, { name, type }) => {
      if (name === 'source' && type === 'change') {
        // onChange fired for the source field
        // reset the "returns" value if the source changes
        setValue('returns', '');
      }
    });
    return () => subscription.unsubscribe();
  }, [watch]);

  const { trackNativeQuery, isPending: isSaving } = useTrackNativeQuery();

  const { hasuraConfirm } = useHasuraAlert();

  const handleFormSubmit = (values: NativeQueryForm) => {
    const metadataNativeQuery = transformFormOutputToMetadata(values);

    trackNativeQuery({
      data: { ...metadataNativeQuery, source: values.source },
      // if this is an "edit", supply the original root_field_name:
      editDetails: editDetails
        ? { rootFieldName: editDetails.nativeQuery.root_field_name }
        : undefined,
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          message: `Successfully ${
            mode === 'create' ? 'tracked' : 'updated'
          } native query as: ${values.root_field_name}`,
          title: 'Track Native Query',
          toastOptions: { duration: 3000 },
        });
        // Go to list
        push(Routes.NativeQueries);
      },
      onError: (err) => {
        hasuraToast({
          type: 'error',
          message: err.message,
          title: 'Track Native Query',
          //toastOptions: { duration: 2000 },
        });
      },
    });
  };

  const { data: allowedDrivers } = useAllDriverCapabilities({
    select: (caps) => caps.map(({ driver }) => driver),
  });

  return (
    <MetadataWrapper
      selector={(m) =>
        m.metadata.sources.filter((s) => allowedDrivers?.includes(s.kind))
      }
      render={({ data: sources }) => (
        <Form onSubmit={handleFormSubmit}>
          <div className="py-2" />
          <NativeQueryFormFields sources={sources} />
          <div className="sticky bottom-0 z-10">
            <Separator size="4" />
            <Flex justify="end" align="center" gap="4" className="p-3">
              <Button
                mode="default"
                variant="ghost"
                type={'button'}
                onClick={() => {
                  if (formHasChanges) {
                    hasuraConfirm({
                      title: 'Unsaved changes!',
                      confirmText: 'Discard Changes',
                      cancelText: 'Stay Here',
                      destructive: true,
                      message:
                        'Are you sure you want to leave this page? Your changes will not be saved.',

                      onClose: ({ confirmed }) => {
                        if (confirmed) push(Routes.NativeQueries);
                      },
                    });
                  } else {
                    push(Routes.NativeQueries);
                  }
                }}
              >
                Cancel
              </Button>
              <Button
                type="submit"
                leftIcon={FaSave}
                mode="primary"
                loading={isSaving}
              >
                {editDetails ? 'Save' : 'Create'}
              </Button>
            </Flex>
          </div>
        </Form>
      )}
    />
  );
};

// this is kind of a distracting bit of logic for the component, so just hiding it here
// it
function useDefaultFormValues({
  editDetails,
  defaultSql,
}: AddNativeQueryProps) {
  const mode = editDetails ? 'update' : 'create';

  const _defaultFormValues: Partial<NativeQueryForm> = React.useMemo(
    () =>
      mode === 'update'
        ? {
            ...editDetails?.nativeQuery,
            source: editDetails?.dataSourceName,
            arguments: normalizeArguments(
              editDetails?.nativeQuery?.arguments ?? {},
            ),
          }
        : {},
    [mode],
  );

  const defaultFormValues = _defaultFormValues;

  if (defaultSql) {
    throw new Error('defaultSql prop is only allowed in Storybook.');
  }

  return {
    mode,
    defaultFormValues,
  };
}
