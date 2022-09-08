import React from 'react';

import z from 'zod';
import { Dialog } from '@/new-components/Dialog';
import { CodeEditorField, Form, InputField } from '@/new-components/Form';
import { SubmitHandler } from 'react-hook-form';
import { QueryCollection } from '@/metadata/types';
import { Tabs } from '@/new-components/Tabs';
import ToolTip from '@/components/Common/Tooltip/Tooltip';
import { GraphQLFileUpload } from './GraphQLFileUpload';

const schema = z.discriminatedUnion('option', [
  z.object({
    option: z.literal('write operation'),
    name: z.string().min(1, 'Name is required'),
    query: z.string().min(1, 'Operation is required'),
  }),
  z.object({
    option: z.literal('upload operation'),
    gqlFile: z.array(
      z.object({
        name: z.string(),
        query: z.string(),
      })
    ),
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

  return (
    <Form
      // '@ts-expect-error' remove this when new form types are available
      onSubmit={onSubmit as SubmitHandler<Record<string, unknown>>}
      schema={schema}
      options={{
        // '@ts-expect-error' remove this when new form types are available
        defaultValues,
      }}
    >
      {options => {
        return (
          <Dialog hasBackdrop title={title} onClose={onClose}>
            <>
              {title === 'Add Operation' ? (
                <Tabs
                  value={tabValue}
                  onValueChange={value => {
                    handleTab();
                    options.setValue('option', value);
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
                    {
                      value: 'upload operation',
                      label: 'Upload Operation',
                      content: (
                        <div className="p-sm overflow-y-auto max-h-[calc(100vh-14rem)]">
                          <div>
                            <label className="flex items-center font-semibold text-muted mb-xs">
                              Upload GraphQL File
                              <ToolTip message=".graphql file with operations" />
                            </label>
                            <GraphQLFileUpload name="gqlFile" />
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
        );
      }}
    </Form>
  );
};
