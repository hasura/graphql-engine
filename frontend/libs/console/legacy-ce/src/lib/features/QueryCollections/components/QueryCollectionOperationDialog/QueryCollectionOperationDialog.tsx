import React from 'react';

import z from 'zod';
import { Dialog } from '../../../../new-components/Dialog';
import {
  CodeEditorField,
  InputField,
  useConsoleForm,
} from '../../../../new-components/Form';
import { SubmitHandler } from 'react-hook-form';
import { QueryCollection } from '../../../../metadata/types';
import { Tabs } from '../../../../new-components/Tabs';
import { QuickAdd } from './QuickAdd';
import { parseQueryString, readFileAsync } from './utils';

type UploadedQueryPayload = {
  gqlFile?: any;
  option: 'upload operation';
};

const schema = z.discriminatedUnion('option', [
  z.object({
    option: z.literal('write operation'),
    name: z.string().min(1, 'Name is required'),
    query: z.string().min(1, 'Operation is required'),
  }),
  z.object({
    option: z.literal('upload operation'),
    gqlFile: z.union([
      z
        .any()
        .refine(files => files?.length === 1, 'GraphQL file is required.')
        .refine(async files => {
          let validQuery = true;
          const data = await readFileAsync(files![0]);
          try {
            parseQueryString(data);
          } catch (error) {
            validQuery = false;
          }
          return validQuery;
        }, 'Invalid GraphQL query'),
      z.array(
        z.object({
          name: z.string(),
          query: z.string(),
        })
      ),
    ]),
  }),
]);

export type QueryCollectionOperation = z.infer<typeof schema>;

interface QueryCollectionOperationDialogProps {
  title: string;
  callToAction: string;
  onSubmit: SubmitHandler<QueryCollectionOperation>;
  onClose: () => void;
  operation?: QueryCollection;
  isLoading: boolean;
  defaultValues: QueryCollectionOperation;
}

export const QueryCollectionOperationDialog = (
  props: QueryCollectionOperationDialogProps
) => {
  const { onClose, title, callToAction, onSubmit, isLoading, defaultValues } =
    props;
  const [tabValue, setTabValue] = React.useState('write operation');

  const handleTab = () => {
    return tabValue === 'write operation'
      ? setTabValue('upload operation')
      : setTabValue('write operation');
  };

  const {
    methods: { setValue },
    Form,
  } = useConsoleForm({
    schema,
    options: {
      defaultValues,
    },
  });

  const handleOnSubmit: SubmitHandler<
    QueryCollectionOperation
  > = async values => {
    const data = { ...values };
    if ((values as UploadedQueryPayload).gqlFile) {
      const gqlFileValue = (values as UploadedQueryPayload).gqlFile;
      (data as UploadedQueryPayload).gqlFile = parseQueryString(
        await readFileAsync(gqlFileValue[0])
      );
    }
    onSubmit(data);
  };

  return (
    <Form onSubmit={handleOnSubmit}>
      <Dialog hasBackdrop title={title} onClose={onClose}>
        <>
          {title === 'Add Operation' ? (
            <Tabs
              value={tabValue}
              onValueChange={value => {
                handleTab();
                if (
                  value === 'write operation' ||
                  value === 'upload operation'
                ) {
                  setValue('option', value);
                }
              }}
              items={[
                {
                  value: 'write operation',
                  label: 'Write Operation',
                  content: (
                    <div className="p-4">
                      <InputField
                        size="full"
                        id="name"
                        name="name"
                        className="max-w-full"
                        label="Operation Name"
                      />
                      <QuickAdd
                        onAdd={operation => {
                          if (operation.name !== 'unnamed') {
                            setValue('name', operation.name);
                          }
                          setValue('query', operation.query);
                        }}
                      />
                      <CodeEditorField
                        id="query"
                        name="query"
                        label="Operation"
                        editorOptions={{
                          minLines: 10,
                          maxLines: 10,
                          showLineNumbers: true,
                        }}
                      />
                    </div>
                  ),
                },
                {
                  value: 'upload operation',
                  label: 'Upload Operation',
                  content: (
                    <div className="p-sm overflow-y-auto max-h-[calc(100vh-14rem)]">
                      <div>
                        <InputField
                          size="full"
                          type="file"
                          id="gqlFile"
                          name="gqlFile"
                          label="Upload GraphQL File"
                          tooltip=".graphql file with operations"
                        />
                      </div>
                    </div>
                  ),
                },
              ]}
            />
          ) : (
            <Tabs
              value={tabValue}
              items={[
                {
                  value: 'write operation',
                  label: 'Write Operation',
                  content: (
                    <div className="p-4">
                      <InputField
                        size="full"
                        id="name"
                        name="name"
                        label="Operation Name"
                      />
                      <CodeEditorField
                        id="query"
                        name="query"
                        label="Operation"
                        editorOptions={{
                          minLines: 10,
                          maxLines: 10,
                          showLineNumbers: true,
                        }}
                      />
                    </div>
                  ),
                },
              ]}
            />
          )}

          <Dialog.Footer
            callToDeny="Cancel"
            callToAction={callToAction}
            onClose={onClose}
            isLoading={isLoading}
          />
        </>
      </Dialog>
    </Form>
  );
};
