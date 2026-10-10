import { useConsoleForm, IndicatorCard, Button } from '@hasura/shared/ui';
import { FormElements } from './FormElements';
import { getDefaultRemoteSchemaToDbValues, schema, Schema } from './schema';
import {
  RelationshipTypeCardRadioGroup,
  RemoteRelOption,
} from '../RemoteSchemaToRemoteSchemaForm/RelationshipTypeCardRadioGroup';
import { useUpsertRemoteSchemaRemoteRelationship } from '@hasura/metadata/api';
import { Flex } from '@radix-ui/themes';
import { RemoteRelationship } from '@hasura/shared/types';

export type RemoteSchemaToDbFormProps = {
  sourceRemoteSchema: string;
  typeName?: string;
  existingRelationship?: RemoteRelationship;
  closeHandler?: () => void;
  onSuccess?: () => void;
  relModeHandler: (v: RemoteRelOption) => void;
};

export const RemoteSchemaToDbForm = ({
  sourceRemoteSchema,
  typeName,
  existingRelationship,
  closeHandler,
  onSuccess,
  relModeHandler,
}: RemoteSchemaToDbFormProps) => {
  const { mutate: upsertRemoteSchemaRemoteRelationship, isPending } =
    useUpsertRemoteSchemaRemoteRelationship();

  const submit = (values: Schema) => {
    const field_mapping: Record<string, string> = values.mapping.reduce(
      (acc, new_value) => {
        acc[new_value.field] = new_value.column;
        return acc;
      },
      {} as Record<string, string>,
    );

    const metadataArgType = existingRelationship ? 'update' : 'create';

    upsertRemoteSchemaRemoteRelationship(
      {
        action: metadataArgType,
        args: {
          remote_schema: sourceRemoteSchema,
          type_name: values.typeName,
          name: values.relationshipName,
          definition: {
            to_source: {
              source: values?.target?.dataSourceName,
              table: values?.target?.table,
              relationship_type: values.relationshipType,
              field_mapping,
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
    schema,
    options: {
      defaultValues: getDefaultRemoteSchemaToDbValues(
        existingRelationship,
        typeName,
      ),
    },
  });

  const formTitle = existingRelationship
    ? 'Edit Relationship'
    : 'Add Relationship';

  return (
    <Form onSubmit={submit} className="p-4">
      <>
        <div className="grid border border-gray-300 rounded shadow-sm p-4">
          <Flex align="center" className="mb-4">
            <Button
              mode="default"
              type="button"
              size="sm"
              onClick={closeHandler}
            >
              Cancel
            </Button>
          </Flex>
          {/* relationship meta */}
          {existingRelationship ? null : (
            <RelationshipTypeCardRadioGroup
              value="remoteDB"
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
              size="2"
              type="submit"
              loading={isPending}
              loadingText="Saving relationship"
              data-test="add-rs-relationship"
            >
              {formTitle}
            </Button>
          </div>
        </div>

        {!!Object.keys(formState.errors).length && (
          <IndicatorCard status="negative">
            Error saving relationship
          </IndicatorCard>
        )}
      </>
    </Form>
  );
};
