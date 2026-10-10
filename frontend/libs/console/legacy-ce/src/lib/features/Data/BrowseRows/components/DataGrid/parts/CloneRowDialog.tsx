import { Dialog } from '@hasura/shared/ui';
import { Source, Table } from '@hasura/shared/types';
import { InsertRowFormContainer } from '../../../../InsertRow/InsertRowFormContainer';

interface CloneRowDialogProps {
  source: Source;
  table: Table;
  row: Record<string, any>;
  onClose: () => void;
}

export const CloneRowDialog = ({
  source,
  table,
  row,
  onClose,
}: CloneRowDialogProps) => (
  <Dialog title="Clone Row" onClose={onClose}>
    <InsertRowFormContainer
      source={source}
      table={table}
      initialValues={row}
      submitLabel="Clone row"
      onSuccess={onClose}
    />
  </Dialog>
);
