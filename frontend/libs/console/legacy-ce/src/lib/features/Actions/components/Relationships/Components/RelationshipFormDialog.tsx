import React from 'react';
import { DelayedDialog, DialogFooter, hasuraToast } from '@hasura/shared/ui';
import { CustomTypeObjectRelationship, Metadata } from '@hasura/shared/types';
import TypeRelationshipEditor from './RelationshipEditor';
import useAddActionRelationship from '../../../../Actions/hooks/useAddActionRelationship';
import { FlattenCustomType } from '../../../../../shared/utils/hasuraCustomTypeUtils';
import {
  getDefaultCustomTypeRelationship,
  getRelValidationError,
  parseCustomTypeRelationship,
} from '../utils';

type Props = {
  outputType: FlattenCustomType;
  existingRelConfig?: CustomTypeObjectRelationship;
  metadata: Metadata['metadata'];
  onClose: () => void;
};

const RelationshipFormDialog = ({
  outputType,
  existingRelConfig,
  metadata,
  onClose,
}: Props) => {
  const [relConfig, setRelConfig] = React.useState(
    existingRelConfig
      ? parseCustomTypeRelationship(existingRelConfig)
      : getDefaultCustomTypeRelationship(),
  );
  const addActionRel = useAddActionRelationship();
  const isPending = false;

  const onSave = () => {
    const validationError = getRelValidationError(relConfig);
    if (validationError) {
      return hasuraToast({
        type: 'error',
        title: 'Cannot create relationship',
        message: validationError,
      });
    }

    addActionRel(
      {
        relConfig,
        existingRelConfig,
        typeName: outputType.definition.name,
        existingTypes: metadata.custom_types ?? {},
      },
      () => {
        onClose();
      },
    );
  };

  return (
    <DelayedDialog
      title={existingRelConfig ? 'Edit Relationship' : 'Add Relationship'}
      description="Configure a relationship for this action's output type"
      onClose={onClose}
      size="lg"
      footer={
        <DialogFooter
          callToDeny="Cancel"
          callToAction={
            existingRelConfig ? 'Save Relationship' : 'Add Relationship'
          }
          onClose={onClose}
          onSubmit={onSave}
          isLoading={isPending}
        />
      }
    >
      <div className="mb-2">
        <TypeRelationshipEditor
          outputType={outputType}
          relConfig={relConfig}
          setRelConfig={setRelConfig}
          metadata={metadata}
          isDisabled={Boolean(existingRelConfig)}
        />
      </div>
    </DelayedDialog>
  );
};

export default RelationshipFormDialog;
