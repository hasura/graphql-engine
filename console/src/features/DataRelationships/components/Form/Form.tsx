import React from 'react';
import { DataTarget } from '@/features/Datasources';
import { Driver } from '@/dataSources';
// eslint-disable-next-line no-restricted-imports
import { Relationship } from '@/features/DataRelationships/components/DatabaseRelationshipsTable/types';
import { FormLayout } from './FormLayout';
import { CreateRelationshipForm } from './CreateRelationshipForm';
import { EditRelationshipForm } from './EditRelationshipForm';

interface Props {
  existingRelationship?: Relationship;
  sourceTableInfo: DataTarget;
  driver: Driver;
  /**
   * optional callback function, can be used to get the onComplete event, this could be a onSuccess, or onError event.
   *
   */
  onComplete: (callback: {
    title?: string;
    message?: string;
    type: 'success' | 'error' | 'cancel';
  }) => void;
  onClose?: () => void;
}

export const Form = ({
  existingRelationship,
  sourceTableInfo,
  onComplete,
  driver,
  onClose,
}: Props) => {
  if (existingRelationship) {
    return (
      <FormLayout existingRelationship onComplete={onComplete}>
        <EditRelationshipForm
          driver={driver}
          sourceTableInfo={sourceTableInfo}
          existingRelationship={existingRelationship}
          onComplete={onComplete}
          onClose={onClose}
        />
      </FormLayout>
    );
  }

  return (
    <FormLayout existingRelationship={false} onComplete={onComplete}>
      <CreateRelationshipForm
        driver={driver}
        sourceTableInfo={sourceTableInfo}
        onComplete={onComplete}
      />
    </FormLayout>
  );
};
