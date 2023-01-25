import { Table } from '@/features/hasura-metadata-types';
import React from 'react';
import { MODE, Relationship } from '../../types';
import { RenameRelationship } from '../RenameRelationship/RenameRelationship';
import { ConfirmDeleteRelationshipPopup } from './parts/ConfirmDeleteRelationshipPopup';
import { CreateRelationship } from './parts/CreateRelationship';

interface RenderWidgetProps {
  dataSourceName: string;
  table: Table;
  mode: MODE;
  relationship?: Relationship;
  onCancel: () => void;
  onSuccess: () => void;
  onError: (err: Error) => void;
}

export const RenderWidget = (props: RenderWidgetProps) => {
  const {
    mode,
    relationship,
    table,
    dataSourceName,
    onSuccess,
    onCancel,
    onError,
  } = props;

  if (mode === MODE.CREATE)
    return (
      <CreateRelationship
        dataSourceName={dataSourceName}
        table={table}
        onSuccess={onSuccess}
        onCancel={onCancel}
        onError={onError}
      />
    );

  if (mode === MODE.RENAME && relationship)
    return (
      <RenameRelationship
        relationship={relationship}
        onSuccess={onSuccess}
        onCancel={onCancel}
        onError={onError}
      />
    );

  if (mode === MODE.DELETE && relationship)
    return (
      <ConfirmDeleteRelationshipPopup
        relationship={relationship}
        onSuccess={onSuccess}
        onCancel={onCancel}
        onError={onError}
      />
    );

  // Since we support only local relationships for GDC, there is no edit mode for it. Edit only exists for remote relationships
  return null;
};
