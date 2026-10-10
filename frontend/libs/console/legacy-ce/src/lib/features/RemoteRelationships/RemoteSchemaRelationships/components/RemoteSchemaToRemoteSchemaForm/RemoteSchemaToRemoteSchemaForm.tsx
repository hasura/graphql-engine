import { Flex } from '@radix-ui/themes';
import {
  useConsoleForm,
  IndicatorCard,
  Button,
  Card,
  Text,
} from '@hasura/shared/ui';
import { useUpsertRemoteSchemaRemoteRelationship } from '@hasura/metadata/api';
import {
  RelationshipTypeCardRadioGroup,
  RemoteRelOption,
} from './RelationshipTypeCardRadioGroup';
import { FormElements } from './FormElements';
import { generateLhsFields } from '../../../utils';
import {
  getDefaultRemoteRelationshipValues,
  rsToRsFormSchema,
  RsToRsSchema,
} from './schemas';
import { RemoteRelationship } from '@hasura/shared/types';

export type RemoteSchemaToRemoteSchemaFormProps = {
  sourceRemoteSchema: string;
  typeName?: string;
  existingRelationship?: RemoteRelationship;
  closeHandler: () => void;
  relModeHandler: (v: RemoteRelOption) => void;
  onSuccess?: () => void;
};

// Wrapper to provide Form Context
export const RemoteSchemaToRemoteSchemaForm = ({
  sourceRemoteSchema,
  typeName,
  existingRelationship,
  closeHandler,
  relModeHandler,
  onSuccess,
}: RemoteSchemaToRemoteSchemaFormProps) => {
  const { mutate: upsertRemoteSchemaRemoteRelationship, isPending } =
    useUpsertRemoteSchemaRemoteRelationship();

  const submit = (values: RsToRsSchema) => {
    const lhs_fields = generateLhsFields(
      values.resultSet as Record<string, unknown>,
    );
    upsertRemoteSchemaRemoteRelationship(
      {
        action: existingRelationship ? 'update' : 'create',
        args: {
          remote_schema: sourceRemoteSchema,
          type_name: values.rsSourceType,
          name: values.name,
          definition: {
            to_remote_schema: {
              remote_schema: values.referenceRemoteSchema,
              lhs_fields,
              remote_field: values.resultSet,
            },
          },
        },
      },
      {
        onSuccess,
      },
    );
  };

  const {
    methods: { formState },
    Form,
  } = useConsoleForm({
    schema: rsToRsFormSchema,
    options: {
      defaultValues: getDefaultRemoteRelationshipValues(
        existingRelationship,
        typeName,
      ),
    },
  });

  const relationshipTitle = existingRelationship
    ? 'Edit Relationship'
    : 'Add Relationship';
  return (
    <Form onSubmit={submit} className="p-4">
      <>
        <Card size="2">
          <Flex align="center" gap="4" className="w-full mb-4">
            <Button
              mode="default"
              type="button"
              size="sm"
              onClick={closeHandler}
            >
              Cancel
            </Button>
            <Text weight="bold">{relationshipTitle}</Text>
          </Flex>

          {existingRelationship ? null : (
            <RelationshipTypeCardRadioGroup
              value="remoteSchema"
              onChange={relModeHandler}
            />
          )}

          <FormElements
            sourceRemoteSchema={sourceRemoteSchema}
            existingRelationship={existingRelationship}
          />

          {/* submit */}
          <div className="mt-4">
            <Button
              mode="primary"
              size="md"
              type="submit"
              loading={isPending}
              loadingText={
                existingRelationship
                  ? 'Updating relationship'
                  : 'Creating relationship'
              }
              data-test="add-rs-relationship"
            >
              {relationshipTitle}
            </Button>
          </div>

          {!!Object.keys(formState.errors).length && (
            <IndicatorCard status="negative">
              Error saving relationship
            </IndicatorCard>
          )}
        </Card>
      </>
    </Form>
  );
};
