import {
  BulkAtomicResponse,
  BulkKeepGoingResponse,
  Table,
} from '../../../hasura-metadata-types';
import { Dialog } from '../../../../new-components/Dialog';
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
  onSuccess: (data: BulkAtomicResponse | BulkKeepGoingResponse) => void;
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
    <Dialog
      hasBackdrop
      title={mode === MODE.EDIT ? 'Edit Relationship' : 'Create Relationship'}
      description={
        mode === MODE.EDIT
          ? 'Edit your existing relationship.'
          : 'Create and track a new relationship to view it in your GraphQL schema.'
      }
      onClose={onCancel}
      size="xxl"
    >
      <div>
        <RelationshipForm.Widget
          dataSourceName={dataSourceName}
          table={table}
          onSuccess={onSuccess}
          onCancel={onCancel}
          onError={onError}
          defaultValue={
            mode === MODE.EDIT && relationship ? relationship : undefined
          }
        />
      </div>
    </Dialog>
  );
};
