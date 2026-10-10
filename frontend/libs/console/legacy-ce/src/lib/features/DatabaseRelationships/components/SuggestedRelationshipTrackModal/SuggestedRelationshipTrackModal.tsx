import React from 'react';
import { z } from 'zod';
import {
  Dialog,
  useConsoleForm,
  GraphQLSanitizedInputField,
  hasuraToast,
  DisplayToastErrorMessage,
  DialogFooter,
} from '@hasura/shared/ui';
import { SuggestedRelationshipWithName } from '../SuggestedRelationships/hooks/useSuggestedRelationships';
import { useCreateTableRelationships } from '@hasura/metadata/data-source';

type SuggestedRelationshipTrackModalProps = {
  relationship: SuggestedRelationshipWithName;
  dataSourceName: string;
  onClose: () => void;
};

export const SuggestedRelationshipTrackModal: React.FC<
  SuggestedRelationshipTrackModalProps
> = ({ relationship, dataSourceName, onClose }) => {
  const { createTableRelationships, isPending } =
    useCreateTableRelationships(dataSourceName);

  const onTrackRelationship = async (relationshipName: string) => {
    createTableRelationships(
      [
        {
          name: relationshipName,
          source: {
            fromSource: dataSourceName,
            fromTable: relationship.from.table,
          },
          definition: {
            target: {
              toSource: dataSourceName,
              toTable: relationship.to.table,
            },
            type: relationship.type,
            detail: {
              fkConstraintOn:
                'constraint_name' in relationship.from
                  ? 'fromTable'
                  : 'toTable',
              fromColumns: relationship.from.columns,
              toColumns: relationship.to.columns,
            },
          },
        },
      ],
      {
        onSuccess: () => {
          hasuraToast({
            type: 'success',
            title: 'Tracked Successfully',
          });
          onClose();
        },
        onError: (err) => {
          hasuraToast({
            type: 'error',
            title: 'Failed to track',
            children: <DisplayToastErrorMessage message={err.message} />,
          });
        },
      },
    );
  };

  const { Form, methods } = useConsoleForm({
    options: {
      defaultValues: {
        relationshipName: relationship.constraintName,
      },
    },
    schema: z.object({
      relationshipName: z
        .string()
        .min(1, 'The relationship name cannot be empty.'),
    }),
  });

  const relationshipName = methods.watch('relationshipName');

  return (
    <Dialog
      title={`Track relationship: ${relationshipName}`}
      description="Add the relationship to the GraphQL API. "
      onClose={onClose}
    >
      <Form onSubmit={(data) => onTrackRelationship(data.relationshipName)}>
        <>
          <div className="my-4">
            <GraphQLSanitizedInputField
              name="relationshipName"
              label="Relationship name"
              tooltip="Relationship names must be unique."
              fieldProps={{ placeholder: 'Relationship name' }}
            />
          </div>
          <DialogFooter
            callToDeny="Cancel"
            callToAction="Track relationship"
            onClose={onClose}
            isLoading={isPending}
          />
        </>
      </Form>
    </Dialog>
  );
};
