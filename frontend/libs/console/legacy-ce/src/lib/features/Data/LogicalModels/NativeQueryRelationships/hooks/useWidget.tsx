import { useCallback, useState } from 'react';
import { useHasuraAlert } from '../../../../../new-components/Alert';
import {
  MetadataSelectors,
  MetadataUtils,
  useMetadata,
} from '../../../../hasura-metadata-api';
import { NativeQueryRelationshipWidget } from '../components';
import { NativeQueryRelationshipFormSchema } from '../schema';

type WidgetMode = 'create' | 'edit';

type onSubmitParams = {
  values: NativeQueryRelationshipFormSchema;
} & (
  | {
      mode: 'edit';
      originalRelationshipName: string;
      originalRelationshipType: 'object' | 'array';
    }
  | { mode: 'create' }
);

// the purpose of this hook is to enclose some of the complexity in working with the relationship widget and give the consumer a simpler API to interact with
export function useWidget({
  nativeQueryName,
  dataSourceName,
  onSubmit,
}: {
  nativeQueryName: string;
  dataSourceName: string;
  onSubmit: (params: onSubmitParams) => void;
}) {
  const [isOpen, setIsOpen] = useState(false);
  const [mode, setMode] = useState<WidgetMode>('create');
  const [defaultValues, setDefaultValues] =
    useState<NativeQueryRelationshipFormSchema>();

  const metadataSelector = useCallback(
    m => {
      const source = MetadataUtils.findMetadataSource(dataSourceName, m);
      if (!source) {
        throw new Error(
          `Unable to find source ${dataSourceName} for Relationship Widget`
        );
      }
      const otherNativeQueries = (source.native_queries ?? []).filter(
        q => q.root_field_name !== nativeQueryName
      );

      const thisNativeQuery = MetadataSelectors.findNativeQuery(
        dataSourceName,
        nativeQueryName
      )(m);

      return {
        otherNativeQueries,
        otherRelationships: {
          object: thisNativeQuery?.object_relationships ?? [],
          array: thisNativeQuery?.array_relationships ?? [],
        },
      };
    },
    [dataSourceName, nativeQueryName]
  );

  const { data: { otherNativeQueries, otherRelationships } = {} } =
    useMetadata(metadataSelector);

  const { hasuraAlert, hasuraConfirm } = useHasuraAlert();

  const openCreate = () => {
    if (!otherNativeQueries || otherNativeQueries.length === 0) {
      alertUserNoPossibleQueries();
      return;
    }
    setMode('create');
    setIsOpen(true);
  };

  const openEdit = (initialValues: NativeQueryRelationshipFormSchema) => {
    setDefaultValues(initialValues);
    setMode('edit');
    setIsOpen(true);
  };

  const closeWidget = () => setIsOpen(false);

  const alertUserNoPossibleQueries = () => {
    hasuraAlert({
      message: (
        <span>
          There are no other native queries to create a relationship with.
          Create another Native Queries on{' '}
          <span className="font-bold">{dataSourceName}</span> and try again.
        </span>
      ),
      title: 'Error',
      onClose: closeWidget,
    });
  };

  const handleSubmit = (values: NativeQueryRelationshipFormSchema) => {
    const doSubmit = () => {
      if (mode === 'edit') {
        if (!defaultValues?.name || !defaultValues.type) {
          throw new Error(
            'Name or type of original relationship was not able to be determined.'
          );
        }
        onSubmit({
          values,
          mode,
          originalRelationshipName: defaultValues.name,
          originalRelationshipType: defaultValues.type,
        });
      } else {
        onSubmit({ values, mode });
      }
    };

    const hasSameName = otherRelationships?.[values.type]?.some(
      r => r.name === values.name
    );

    const isCreating = mode === 'create';

    const changedTypeWhileEditing =
      mode === 'edit' && defaultValues?.type !== values.type;

    const warnOfOverwrite =
      hasSameName && (isCreating || changedTypeWhileEditing);

    if (warnOfOverwrite) {
      hasuraConfirm({
        title: 'Overwrite Existing Relationship?',
        message: (
          <div>
            There is already an existing <strong>{values.type}</strong>{' '}
            relationship with the name <strong>{values.name}</strong>. Do you
            want to overwrite this relationship?
          </div>
        ),
        confirmText: 'Overwrite',
        destructive: true,
        onClose: ({ confirmed }) => {
          if (confirmed) doSubmit();
        },
      });
    } else {
      doSubmit();
    }
  };

  const WidgetUI = () => (
    <>
      {isOpen && (
        <NativeQueryRelationshipWidget
          fromNativeQuery={nativeQueryName}
          dataSourceName={dataSourceName}
          defaultValues={mode === 'create' ? undefined : defaultValues}
          mode={mode}
          asDialog
          onSubmit={handleSubmit}
          onCancel={() => setIsOpen(false)}
        />
      )}
    </>
  );

  return {
    WidgetUI,
    closeWidget,
    openCreate,
    openEdit,
  };
}
