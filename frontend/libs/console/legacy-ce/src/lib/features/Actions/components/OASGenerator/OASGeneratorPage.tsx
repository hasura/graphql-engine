import { generatedActionToHasuraAction } from '../OASGenerator/utils';
import { GeneratedAction } from './types';
import { FaFileImport, FaHome } from 'react-icons/fa';
import { z } from 'zod';
import { useLocalStorage } from '@hasura/shared/hooks';
import { SimpleForm, Text, Breadcrumbs } from '@hasura/shared/ui';
import { useMetadata } from '@hasura/metadata/api';
import { OasGeneratorForm } from './OASGeneratorForm';
import useCreateActionMigration from '../../hooks/useCreateAction';
import useDeleteAction from '../../hooks/useDeleteAction';
import { Heading } from '@radix-ui/themes';

export const formSchema = z.object({
  oas: z.string(),
  url: z
    .string()
    .url({ message: 'Invalid URL' })
    .refine((val) => !val.endsWith('/'), {
      message: "Base URL can't end with a slash",
    }),
  search: z.string(),
});

export const OASGeneratorPage = () => {
  const { data: meta } = useMetadata();
  const { createAction, isPending: isCreating } = useCreateActionMigration();
  const { deleteAction, isPending: isDeleting } = useDeleteAction();

  const [savedOas, setSavedOas] = useLocalStorage<string>('oas', '');

  const onGenerate = (action: GeneratedAction) => {
    if (!meta?.metadata) {
      return;
    }
    const { state, requestTransform, responseTransform } =
      generatedActionToHasuraAction(action);
    createAction({
      metadata: meta.metadata,
      rawState: state,
      requestTransform,
      responseTransform,
    }).catch(() => {
      //
    });
  };

  const onDelete = (actionName: string) => {
    const action = meta?.metadata?.actions?.find(
      (a) => a.name.toLowerCase() === actionName.toLowerCase(),
    );
    if (action) {
      deleteAction(action.name);
    }
  };

  return (
    <div className="mt-6">
      <div className="mb-4">
        <div>
          <Breadcrumbs
            items={[
              {
                url: '/actions',
                icon: <FaHome />,
                title: 'Actions',
              },
              {
                icon: <FaFileImport />,
                title: 'Import OpenAPI',
              },
            ]}
          />
          <div className="mt-4">
            <Heading size="6">Import from OpenAPI spec</Heading>
          </div>
          <Text>
            Import a REST endpoint as an Action from an OpenAPI (OAS3) spec.
          </Text>
        </div>
      </div>
      <SimpleForm
        onSubmit={() => {
          /* noop */
        }}
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
          disabled={isCreating || isDeleting}
          saveOas={setSavedOas}
        />
      </SimpleForm>
    </div>
  );
};
