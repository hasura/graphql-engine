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
  const { mode, relationship, table, dataSourceName, ...callbacks } = props;

  if (mode === MODE.CREATE)
    return (
      <CreateRelationship
        dataSourceName={dataSourceName}
        table={table}
        {...callbacks}
      />
    );

  if (mode === MODE.RENAME && relationship)
    return <RenameRelationship relationship={relationship} {...callbacks} />;

  if (mode === MODE.DELETE && relationship)
    return (
      <ConfirmDeleteRelationshipPopup
        relationship={relationship}
        {...callbacks}
      />
    );

  // Since we support only local relationships for GDC, there is no edit mode for it. Edit only exists for remote relationships
  return null;
};
