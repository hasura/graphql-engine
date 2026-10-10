import { Table } from '@hasura/shared/types';
import { MODE, Relationship } from '../../types';
import { RelationshipForm } from '../RelationshipForm';
import { RenameRelationship } from '../RenameRelationship/RenameRelationship';
import { ConfirmDeleteRelationshipPopup } from './parts/ConfirmDeleteRelationshipPopup';

interface RenderWidgetProps {
  dataSourceName: string;
  table: Table;
  mode: MODE;
  relationship?: Relationship;
  onCancel: () => void;
  onSuccess: (data: unknown) => void;
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

  if (mode === MODE.DELETE && relationship)
    return (
      <ConfirmDeleteRelationshipPopup
        relationship={relationship}
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

  return (
    <RelationshipForm.Widget
      dataSourceName={dataSourceName}
      mode={mode}
      table={table}
      onSuccess={onSuccess}
      onCancel={onCancel}
      onError={onError}
      defaultValue={
        mode === MODE.EDIT && relationship ? relationship : undefined
      }
    />
  );
};
