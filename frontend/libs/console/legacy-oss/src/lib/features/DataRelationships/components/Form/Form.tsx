import React from 'react';

import { RowData } from '@/features/RelationshipsTable';
import { DataTarget } from '@/features/Datasources';
import { Driver } from '@/dataSources';

import { FormLayout } from './FormLayout';
import { CreateRelationshipForm } from './CreateRelationshipForm';
import { EditRelationshipForm } from './EditRelationshipForm';

interface Props {
  existingRelationship?: RowData;
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
}

export const Form = ({
  existingRelationship,
  sourceTableInfo,
  onComplete,
  driver,
}: Props) => {
  if (existingRelationship) {
    return (
      <FormLayout existingRelationship onComplete={onComplete}>
        <EditRelationshipForm
          driver={driver}
          sourceTableInfo={sourceTableInfo}
          existingRelationship={existingRelationship}
          onComplete={onComplete}
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
