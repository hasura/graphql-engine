import {
  Button,
  InputField,
  useConsoleForm,
  IconTooltip,
  RequestHeadersSelector,
  Text,
  CheckboxField,
  FieldLabel,
} from '@hasura/shared/ui';
import { FieldError } from 'react-hook-form';
import get from 'lodash/get';
import { useState } from 'react';
import { FaPlusCircle } from 'react-icons/fa';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { schema, Schema } from './schema';
import { transformFormData } from './utils';
import { GraphQLServiceUrl } from './GraphQLServiceUrl';
import { Flex, Heading } from '@radix-ui/themes';
import type {
  RemoteSchema,
  RemoteSchemaCustomization,
} from '@hasura/shared/types';
import GraphQLCustomizations from './GraphQLCustomizations';

type Props = {
  defaultValues: Schema;
  edit?: boolean;
  saving: boolean;
  deleting?: boolean;
  onSubmit: (values: RemoteSchema) => Promise<unknown>;
  /**
   * The remote schema's current customization (when editing). Parts of it the
   * form doesn't edit, like type/field name mappings, are kept on save.
   */
  existingCustomization?: RemoteSchemaCustomization;
  onDelete?: React.MouseEventHandler<HTMLButtonElement>;
};

const RemoteSchemaForm = ({
  defaultValues,
  onSubmit,
  edit,
  onDelete,
  saving,
  deleting,
  existingCustomization,
}: Props) => {
  const [openCustomizationWidget, setOpenCustomizationWidget] = useState(
    () =>
      !!(
        defaultValues?.customization?.root_fields_namespace ||
        defaultValues?.customization?.type_prefix ||
        defaultValues?.customization?.type_suffix
      ),
  );

  const {
    methods: { formState, setValue },
    Form,
  } = useConsoleForm({
    schema,
    options: {
      defaultValues,
    },
  });

  const handleSubmit = (values) => {
    const args = transformFormData(values, existingCustomization);
    onSubmit(args);
  };

  const queryRootError = get(formState.errors, 'customization.query_root') as
    FieldError | undefined;

  const mutationRootError = get(
    formState.errors,
    'customization.mutation_root',
  ) as FieldError | undefined;

  const disabled = saving || deleting;

  return (
    <Form onSubmit={handleSubmit} className="p-4">
      <Analytics name="RemoteSchemaForm" {...REDACT_EVERYTHING}>
        <div className="max-w-6xl mb-4">
          {!edit && (
            <div className="mb-6">
              <Heading size="4">Add Remote Schema</Heading>
            </div>
          )}
          <div className="w-8/12">
            <InputField
              name="name"
              label="Remote Schema Name"
              tooltip="give this GraphQL schema a friendly name"
              fieldProps={{
                placeholder: 'Name...',
                disabled: edit || disabled,
              }}
            />
          </div>
          <div className="w-8/12">
            <InputField
              name="comment"
              label="Comment / Description"
              tooltip="A statement to help describe the remote schema in brief"
              fieldProps={{
                placeholder: 'Comment / Description...',
                disabled: disabled,
              }}
            />
          </div>
          <GraphQLServiceUrl />
          <div className="w-4/12">
            <InputField
              label="GraphQL Server Timeout"
              tooltip="Configure timeout for your remote GraphQL server. Defaults to 60 seconds."
              name="timeout_seconds"
              data-testid="timeout_seconds"
              fieldProps={{
                type: 'number',
                disabled,
                appendLabel: 'Seconds',
              }}
            />
          </div>
          <div className="w-8/12">
            <Heading size="4">Headers</Heading>
            <div className="my-2">
              <CheckboxField
                name="forward_client_headers"
                data-testid="forward_client_headers"
                disabled={disabled}
              >
                <Flex align="center" gap="2">
                  Forward all headers from client
                  <IconTooltip
                    message="Toggle forwarding headers sent by the client app in the request to
          your remote GraphQL server"
                  />
                </Flex>
              </CheckboxField>
            </div>

            <FieldLabel
              className="mb-2"
              label="Request headers:"
              tooltip="Headers sent when executing GraphQL operations against the Remote Schema"
              description="Headers sent when executing GraphQL operations against the
                Remote Schema."
            />
            <RequestHeadersSelector
              name="headers"
              addButtonText="Add additional headers"
              disabled={disabled}
            />

            <FieldLabel
              className="my-4"
              label="Introspection headers:"
              tooltip="Headers used to fetch the Remote Schema GraphQL schema. If none are added, the request headers configured above are reused."
              description="Headers used to fetch the Remote Schema GraphQL schema. If none are added, the request headers configured above are reused."
            />

            <RequestHeadersSelector
              name="introspection_headers"
              addButtonText="Add introspection headers"
              disabled={disabled}
              onAdd={() =>
                setValue('use_introspection_headers', true, {
                  shouldDirty: true,
                })
              }
            />
          </div>
          <div className="my-4 xl:w-8/12">
            <Heading size="4">GraphQL Customizations</Heading>
            <Text>
              Individual Types and Fields will be editable after saving.
              <br />
              <a href="https://spec.graphql.org/June2018/#example-e2969">
                Read more
              </a>{' '}
              about Type and Field naming conventions in the official GraphQL
              spec
            </Text>

            <div className="mt-4">
              {openCustomizationWidget ? (
                <GraphQLCustomizations
                  mutationRootError={mutationRootError}
                  onClose={() => setOpenCustomizationWidget(false)}
                  queryRootError={queryRootError}
                  disabled={disabled}
                />
              ) : (
                <Button
                  size="1"
                  leftIcon={FaPlusCircle}
                  type="button"
                  mode="default"
                  onClick={() => setOpenCustomizationWidget(true)}
                  data-testid="open_customization"
                  disabled={disabled}
                >
                  Add GQL Customization
                </Button>
              )}
            </div>
          </div>
          <Flex align="center" gap="2" className="mt-2">
            <Button
              type="submit"
              data-testid="submit"
              mode="primary"
              loading={saving}
              disabled={disabled}
            >
              {edit ? 'Save' : 'Create'} Remote Schema
            </Button>
            {edit ? (
              <Button
                type="button"
                data-testid="delete"
                mode="destructive"
                loading={deleting}
                onClick={onDelete}
                disabled={disabled}
              >
                Delete
              </Button>
            ) : null}
          </Flex>
        </div>
      </Analytics>
    </Form>
  );
};

export default RemoteSchemaForm;
