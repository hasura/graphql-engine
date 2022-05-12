import { useMetadataMigration } from '@/features/MetadataAPI';
import { Button } from '@/new-components/Button';
import { FieldError } from 'react-hook-form';
import { Form, InputField } from '@/new-components/Form';
import { fireNotification } from '@/new-components/Notifications';
import { ToolTip } from '@/new-components/Tooltip';
import get from 'lodash.get';
import { APIError } from '@/hooks/error';
import React, { useState } from 'react';
import { FaExclamationCircle, FaPlusCircle } from 'react-icons/fa';
import { Headers } from '../Headers';
import { schema, Schema } from './schema';
import { transformFormData } from './utils';

type Props = {
  onSuccess?: (remoteSchemaName?: string) => void;
};

export const Create = ({ onSuccess }: Props) => {
  const [remoteSchemaName, setRemoteSchemaName] = useState('');

  const mutation = useMetadataMigration({
    onError: (error: APIError) => {
      fireNotification({
        type: 'error',
        title: 'Error',
        message: error?.message ?? 'Unable to create Remote Schema',
      });
    },
    onSuccess: () => {
      fireNotification({
        type: 'success',
        title: 'Success!',
        message: 'Remote Schema created successfully',
      });

      if (onSuccess) onSuccess(remoteSchemaName);
    },
  });

  const onSubmit = (values: Schema) => {
    const args = transformFormData(values);
    setRemoteSchemaName(values.name);

    mutation.mutate({
      query: { type: 'add_remote_schema', args },
    });
  };
  const defaultValues: Schema = {
    name: '',
    url: '',
    headers: [],
    forward_client_headers: false,
    timeout_seconds: '',
    comment: '',
    customization: {
      root_fields_namespace: '',
      type_prefix: '',
      type_suffix: '',
      query_root: {
        parent_type: '',
        prefix: '',
        suffix: '',
      },
      mutation_root: {
        parent_type: '',
        prefix: '',
        suffix: '',
      },
    },
  };

  const [openCustomizationWidget, setOpenCustomizationWidget] = useState(false);

  return (
    <Form
      schema={schema}
      onSubmit={onSubmit}
      options={{ defaultValues }}
      className="overflow-y-hidden p-4"
    >
      {options => {
        const queryRootError = get(
          options.formState.errors,
          'customization.query_root'
        ) as FieldError | undefined;

        const mutationRootError = get(
          options.formState.errors,
          'customization.mutation_root'
        ) as FieldError | undefined;

        return (
          <div className="max-w-6xl">
            <h1 className="text-xl leading-6 font-semibold mb-lg">
              Add Remote Schema
            </h1>
            <div className="mb-md w-6/12">
              <InputField
                name="name"
                label="Remote Schema Name"
                placeholder="Name..."
                tooltip="give this GraphQL schema a friendly name"
              />
            </div>
            <div className="mb-md w-6/12">
              <InputField
                name="comment"
                label="Comment / Description"
                placeholder="Comment / Description..."
                tooltip="A statement to help describe the remote schema in brief"
              />
            </div>
            <div className="mb-md w-6/12">
              <InputField
                name="url"
                label="GraphQL Service URL"
                placeholder="https://myservice.com/graphql"
                description="Note: Specifying the server URL via an environmental variable is
                recommended if you have different URLs for multiple
                environments."
                tooltip="Remote GraphQL server’s URL. E.g. https://my-domain/v1/graphql"
              />
            </div>
            <div className="mb-lg w-4/12">
              <label className="block flex items-center text-gray-600 font-semibold mb-xs">
                GraphQL Server Timeout
                <ToolTip message="Configure timeout for your remote GraphQL server. Defaults to 60 seconds." />
              </label>
              <div className="relative w-full">
                <input
                  type="text"
                  className="block w-full shadow-sm rounded pr-10 border-gray-300 hover:border-gray-400 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
                  placeholder="60"
                  {...options.register('timeout_seconds')}
                  data-testid="timeout_seconds"
                />
                <div className="absolute inset-y-0 right-0 pr-3 flex text-gray-400 items-center pointer-events-none">
                  Seconds
                </div>
              </div>
            </div>
            <div className="mb-lg w-8/12">
              <h2 className="text-lg font-semibold text-gray-600 ">Headers</h2>

              <div className="items-center mr-sm mb-sm my-sm flex">
                <input
                  {...options.register('forward_client_headers')}
                  type="checkbox"
                  className="mr-sm border-gray-400 rounded focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-yellow-400"
                  value="true"
                  data-testid="forward_client_headers"
                />
                <label className="pl-3 flex items-center">
                  Forward all headers from client
                  <ToolTip message="Custom headers to be sent to the remote GraphQL server" />
                </label>
              </div>

              <Headers name="headers" />
            </div>
            <div className="mb-lg w-8/12">
              <h2 className="text-lg font-semibold flex items-center">
                GraphQL Customizations
              </h2>
              <p className="text-sm text-gray-600 mb-sm">
                Individual Types and Fields will be editable after saving.
                <br />
                <a href="https://spec.graphql.org/June2018/#example-e2969">
                  Read more
                </a>{' '}
                about Type and Field naming conventions in the official GraphQL
                spec
              </p>

              {openCustomizationWidget ? (
                <div className="w-full rounded border bg-white border-gray-300 p-4">
                  <Button
                    type="button"
                    size="sm"
                    onClick={() => setOpenCustomizationWidget(false)}
                  >
                    Close
                  </Button>

                  <div className="grid gap-3 grid-cols-12 mb-md">
                    <div className="flex items-center col-span-4">
                      <label className="block text-gray-600 font-medium">
                        Root Field Namespace
                      </label>
                    </div>
                    <div className="col-span-8">
                      <input
                        type="text"
                        className="w-full block h-10 shadow-sm rounded border-gray-300 hover:border-gray-400 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
                        placeholder="namespace_"
                        {...options.register(
                          'customization.root_fields_namespace'
                        )}
                        data-testid="customization.root_fields_namespace"
                      />
                    </div>
                  </div>

                  <h2 className="text-lg font-semibold mb-xs items-center flex">
                    Types
                    <ToolTip message="add a prefix / suffix to all types of the remote schema" />
                  </h2>

                  <div className="grid gap-3 grid-cols-12 mb-md">
                    <div className="flex items-center col-span-4">
                      <label className="block text-gray-600 font-medium">
                        Prefix
                      </label>
                    </div>
                    <div className="col-span-8">
                      <input
                        type="text"
                        className="w-full block h-10 shadow-sm rounded border-gray-300 hover:border-gray-400 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
                        placeholder="prefix_"
                        {...options.register('customization.type_prefix')}
                        data-testid="customization.type_prefix"
                      />
                    </div>
                  </div>
                  <div className="grid gap-3 grid-cols-12 mb-md">
                    <div className="flex items-center col-span-4">
                      <label className="block text-gray-600 font-medium">
                        Suffix
                      </label>
                    </div>
                    <div className="col-span-8">
                      <input
                        type="text"
                        className="w-full block h-10 shadow-sm rounded border-gray-300 hover:border-gray-400 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
                        placeholder="_suffix"
                        {...options.register('customization.type_suffix')}
                        data-testid="customization.type_suffix"
                      />
                    </div>
                  </div>

                  <h2 className="text-lg font-semibold mb-xs flex items-center">
                    Fields
                    <ToolTip message="add a prefix / suffix to the fields of the query / mutation root fields" />
                  </h2>

                  <h3 className="font-semibold mb-xs text-gray-600 text-lg">
                    Query root
                  </h3>
                  {queryRootError?.message && (
                    <div
                      role="alert"
                      aria-label={queryRootError.message}
                      className="mt-xs text-red-600 flex items-center"
                    >
                      <FaExclamationCircle className="fill-current h-4 mr-xs" />
                      {queryRootError.message}
                    </div>
                  )}
                  <div className="grid gap-3 grid-cols-12 mb-md">
                    <div className="flex items-center col-span-4">
                      <label className="block text-gray-600 font-medium">
                        Type Name
                      </label>
                    </div>
                    <div className="col-span-8">
                      <input
                        type="text"
                        className="w-full block h-10 shadow-sm rounded border-gray-300 hover:border-gray-400 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
                        placeholder="Query/query_root"
                        {...options.register(
                          'customization.query_root.parent_type'
                        )}
                        data-testid="customization.query_root.parent_type"
                      />
                    </div>
                  </div>

                  <div className="grid gap-3 grid-cols-12 mb-md">
                    <div className="flex items-center col-span-4">
                      <label className="block text-gray-600 font-medium">
                        Prefix
                      </label>
                    </div>
                    <div className="col-span-8">
                      <input
                        type="text"
                        className="w-full block h-10 shadow-sm rounded border-gray-300 hover:border-gray-400 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
                        placeholder="prefix_"
                        {...options.register('customization.query_root.prefix')}
                        data-testid="customization.query_root.prefix"
                      />
                    </div>
                  </div>

                  <div className="grid gap-3 grid-cols-12 mb-md">
                    <div className="flex items-center col-span-4">
                      <label className="block text-gray-600 font-medium">
                        Suffix
                      </label>
                    </div>
                    <div className="col-span-8">
                      <input
                        type="text"
                        className="w-full block h-10 shadow-sm rounded border-gray-300 hover:border-gray-400 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
                        placeholder="_suffix"
                        {...options.register('customization.query_root.suffix')}
                        data-testid="customization.query_root.suffix"
                      />
                    </div>
                  </div>

                  <h3 className="font-semibold mb-xs text-gray-600 text-lg">
                    Mutation root
                  </h3>
                  {mutationRootError?.message && (
                    <div
                      role="alert"
                      aria-label={mutationRootError.message}
                      className="mt-xs text-red-600 flex items-center"
                    >
                      <FaExclamationCircle className="fill-current h-4 mr-xs" />
                      {mutationRootError.message}
                    </div>
                  )}

                  <div className="grid gap-3 grid-cols-12 mb-md">
                    <div className="flex items-center col-span-4">
                      <label className="block text-gray-600 font-medium">
                        Type Name
                      </label>
                    </div>
                    <div className="col-span-8">
                      <input
                        type="text"
                        className="w-full block h-10 shadow-sm rounded border-gray-300 hover:border-gray-400 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
                        placeholder="Mutation/mutation_root"
                        {...options.register(
                          'customization.mutation_root.parent_type'
                        )}
                        data-testid="customization.mutation_root.parent_type"
                      />
                    </div>
                  </div>

                  <div className="grid gap-3 grid-cols-12 mb-md">
                    <div className="flex items-center col-span-4">
                      <label className="block text-gray-600 font-medium">
                        Prefix
                      </label>
                    </div>
                    <div className="col-span-8">
                      <input
                        type="text"
                        className="w-full block h-10 shadow-sm rounded border-gray-300 hover:border-gray-400 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
                        placeholder="prefix_"
                        {...options.register(
                          'customization.mutation_root.prefix'
                        )}
                        data-testid="customization.mutation_root.prefix"
                      />
                    </div>
                  </div>
                  <div className="grid gap-3 grid-cols-12">
                    <div className="flex items-center col-span-4">
                      <label className="block text-gray-600 font-medium">
                        Suffix
                      </label>
                    </div>
                    <div className="col-span-8">
                      <input
                        type="text"
                        className="w-full block h-10 shadow-sm rounded border-gray-300 hover:border-gray-400 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
                        placeholder="_suffix"
                        {...options.register(
                          'customization.mutation_root.suffix'
                        )}
                        data-testid="customization.mutation_root.suffix"
                      />
                    </div>
                  </div>
                </div>
              ) : (
                <Button
                  icon={<FaPlusCircle />}
                  type="button"
                  size="sm"
                  onClick={() => setOpenCustomizationWidget(true)}
                  data-testid="open_customization"
                >
                  Add GQL Customization
                </Button>
              )}
            </div>
            <div className="flex items-center mb-lg">
              <Button
                type="submit"
                data-testid="submit"
                mode="primary"
                isLoading={mutation.isLoading}
              >
                Add Remote Schema
              </Button>
            </div>
          </div>
        );
      }}
    </Form>
  );
};
