import React from 'react';
import { z } from 'zod';
import { Dialog } from '../../../../new-components/Dialog';
import {
  useConsoleForm,
  GraphQLSanitizedInputField,
} from '../../../../new-components/Form';
import { hasuraToast } from '../../../../new-components/Toasts';
import { SuggestedRelationshipWithName } from '../SuggestedRelationships/hooks/useSuggestedRelationships';
import { useCreateTableRelationships } from '../../hooks/useCreateTableRelationships/useCreateTableRelationships';
import { DisplayToastErrorMessage } from '../../../Data/components/DisplayErrorMessage';
import { useAppDispatch } from '../../../../storeHooks';
import { updateSchemaInfo } from '../../../../components/Services/Data/DataActions';
import { MetadataSelectors, useMetadata } from '../../../hasura-metadata-api';

type SuggestedRelationshipTrackModalProps = {
  relationship: SuggestedRelationshipWithName;
  dataSourceName: string;
  onClose: () => void;
};

export const SuggestedRelationshipTrackModal: React.VFC<
  SuggestedRelationshipTrackModalProps
> = ({ relationship, dataSourceName, onClose }) => {
  const dispatch = useAppDispatch();
  const { data: driver } = useMetadata(
    m => MetadataSelectors.findSource(dataSourceName)(m)?.kind
  );

  const isLoadSchemaRequired = driver === 'mssql' || driver === 'postgres';

  const { createTableRelationships, isLoading } = useCreateTableRelationships(
    dataSourceName,
    {
      onSuccess: () => {
        if (isLoadSchemaRequired) {
          dispatch(updateSchemaInfo());
        }
      },
    }
  );

  const onTrackRelationship = async (relationshipName: string) => {
    createTableRelationships({
      data: [
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
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: 'Tracked Successfully',
        });
        onClose();
      },
      onError: err => {
        hasuraToast({
          type: 'error',
          title: 'Failed to track',
          children: <DisplayToastErrorMessage message={err.message} />,
        });
      },
    });
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
            isLoading={isLoading}
          />
        </>
      </Form>
    </Dialog>
  );
};
