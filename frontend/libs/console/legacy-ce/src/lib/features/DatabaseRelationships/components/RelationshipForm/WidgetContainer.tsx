import {
  BulkAtomicResponse,
  BulkKeepGoingResponse,
  Table,
} from '@hasura/shared/types';
import { Dialog, Spinner } from '@hasura/shared/ui';
import { MODE, Relationship } from '../../types';
import { Widget } from './Widget';
import { useSourceOptions } from '../../hooks/useSourceOptions';

interface WidgetProps {
  mode: MODE;
  relationship?: Relationship;
  dataSourceName: string;
  table: Table;
  onCancel: () => void;
  onSuccess: (data: BulkAtomicResponse | BulkKeepGoingResponse) => void;
  onError: (err: Error) => void;
  defaultValue?: Relationship;
}

export const WidgetContainer = ({
  mode,
  relationship,
  table,
  dataSourceName,
  onSuccess,
  onCancel,
  onError,
}: WidgetProps) => {
  const { sourceOptions, inconsistentSources } = useSourceOptions();

  if (!sourceOptions) {
    return <Spinner />;
  }

  return (
    <Dialog
      title={mode === MODE.EDIT ? 'Edit Relationship' : 'Create Relationship'}
      description={
        mode === MODE.EDIT
          ? 'Edit your existing relationship.'
          : 'Create and track a new relationship to view it in your GraphQL schema.'
      }
      onClose={onCancel}
      size="xxl"
      open
    >
      <div>
        <Widget
          sourceOptions={sourceOptions}
          inconsistentSources={inconsistentSources}
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
