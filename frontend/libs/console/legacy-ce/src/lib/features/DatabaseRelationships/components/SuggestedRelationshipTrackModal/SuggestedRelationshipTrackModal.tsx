import React from 'react';
import { z } from 'zod';
import { Dialog } from '../../../../new-components/Dialog';
import {
  useConsoleForm,
  GraphQLSanitizedInputField,
} from '../../../../new-components/Form';
import { hasuraToast } from '../../../../new-components/Toasts';
import {
  SuggestedRelationshipWithName,
  useSuggestedRelationships,
} from '../SuggestedRelationships/hooks/useSuggestedRelationships';

type SuggestedRelationshipTrackModalProps = {
  relationship: SuggestedRelationshipWithName;
  dataSourceName: string;
  onClose: () => void;
};

export const SuggestedRelationshipTrackModal: React.VFC<
  SuggestedRelationshipTrackModalProps
> = ({ relationship, dataSourceName, onClose }) => {
  const {
    onAddSuggestedRelationship,
    isAddingSuggestedRelationship,
    refetchSuggestedRelationships,
  } = useSuggestedRelationships({
    dataSourceName,
    table: relationship.from.table,
    existingRelationships: [],
    isEnabled: true,
  });

  const onTrackRelationship = async (relationshipName: string) => {
    try {
      await onAddSuggestedRelationship({
        ...relationship,
        constraintName: relationshipName,
      });

      refetchSuggestedRelationships();
      onClose();
    } catch (err: unknown) {
      hasuraToast({
        title: 'Error',
        message: err instanceof Error ? err.message : 'An error occurred',
        type: 'error',
      });
    }
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
      hasBackdrop
      title={`Track relationship: ${relationshipName}`}
      description="Add the relationship to the GraphQL API. "
      onClose={onClose}
    >
      <Form onSubmit={data => onTrackRelationship(data.relationshipName)}>
        <>
          <div className="m-4">
            <GraphQLSanitizedInputField
              name="relationshipName"
              label="Relationship name"
              placeholder="Relationship name"
              tooltip="Relationship names must be unique."
            />
          </div>
          <Dialog.Footer
            callToDeny="Cancel"
            callToAction="Track relationship"
            onClose={onClose}
            isLoading={isAddingSuggestedRelationship}
          />
        </>
      </Form>
    </Dialog>
  );
};
