import { useDispatch, useStore } from 'react-redux';
import { Link } from 'react-router';
import {
  createActionMigration,
  deleteAction,
  executeActionCreation,
} from '../../../../components/Services/Actions/ServerIO';
import { parseCustomTypes } from '../../../../shared/utils/hasuraCustomTypeUtils';
import { useMetadata } from '../../../MetadataAPI';
import { generatedActionToHasuraAction } from '../OASGenerator/utils';
import { GeneratedAction } from './types';

import React from 'react';
import { FaAngleRight, FaFileImport, FaHome } from 'react-icons/fa';
import { z } from 'zod';
import { useLocalStorage } from '../../../../hooks';
import { SimpleForm } from '../../../../new-components/Form';
import { useInvalidateMetadata } from '../../../hasura-metadata-api';
import { OasGeneratorForm } from './OASGeneratorForm';

export const formSchema = z.object({
  oas: z.string(),
  url: z
    .string()
    .url({ message: 'Invalid URL' })
    .refine(val => !val.endsWith('/'), {
      message: "Base URL can't end with a slash",
    }),
  search: z.string(),
});

export const Breadcrumbs = () => (
  <div className="flex items-center space-x-xs mb-4">
    <Link
      to="/actions"
      className="cursor-pointer flex items-center text-muted hover:text-gray-900"
    >
      <FaHome className="mr-1.5" />
      <span className="text-sm">Actions</span>
    </Link>
    <FaAngleRight className="text-muted" />
    <div className="cursor-pointer flex items-center text-yellow-500">
      <FaFileImport className="mr-1.5" />
      <span className="text-sm">Import OpenAPI</span>
    </div>
  </div>
);

export const OASGeneratorPage = () => {
  const dispatch = useDispatch();
  const store = useStore();
  const invalidateMetadata = useInvalidateMetadata();

  const [savedOas, setSavedOas] = useLocalStorage<string>('oas', '');

  const { data: metadata } = useMetadata();
  const [busy, setBusy] = React.useState(false);

  const onGenerate = (action: GeneratedAction) => {
    if (metadata) {
      const { state, requestTransform, responseTransform } =
        generatedActionToHasuraAction(action);
      const actionMigration = createActionMigration(
        state,
        parseCustomTypes(metadata.metadata.custom_types ?? {}),
        metadata.metadata.actions ?? [],
        requestTransform,
        responseTransform
      );
      if (actionMigration) {
        setBusy(true);
        executeActionCreation(
          dispatch,
          store.getState,
          actionMigration.name,
          actionMigration.migration,
          state,
          false,
          () => {
            invalidateMetadata({
              componentName: 'OASGeneratorPage',
              reasons: ['onGenerate action migration occurred'],
            });
            setBusy(false);
          },
          () => {
            setBusy(false);
          }
        );
      }
    }
  };

  const onDelete = (actionName: string) => {
    const action = metadata?.metadata?.actions?.find(
      a => a.name.toLowerCase() === actionName.toLowerCase()
    );
    if (action) {
      setBusy(true);
      deleteAction(action)(
        dispatch,
        store.getState,
        false,
        () => {
          invalidateMetadata({
            componentName: 'OASGeneratorPage',
            reasons: ['onDelete delete action occurred'],
          });
          setBusy(false);
        },
        () => {
          setBusy(false);
        }
      );
    }
  };

  return (
    <div>
      <div>
        <div className="border-b mb-8 border-solid border-gray-200 -mx-4 px-4 pb-6">
          <Breadcrumbs />
          <h1 className="text-xl font-semibold">Import from OpenAPI spec</h1>
          <p className="text-muted m-0">
            Import a REST endpoint as an Action from an OpenAPI (OAS3) spec.
          </p>
        </div>
      </div>
      <SimpleForm
        onSubmit={() => {}}
        schema={formSchema}
        options={{
          defaultValues: {
            oas: savedOas || '',
          },
        }}
      >
        <OasGeneratorForm
          onGenerate={onGenerate}
          onDelete={onDelete}
          disabled={busy}
          saveOas={setSavedOas}
        />
      </SimpleForm>
    </div>
  );
};
