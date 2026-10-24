import { Dialog } from '@hasura/shared/ui';
import { Source, Table } from '@hasura/shared/types';
import { EditRowFormContainer } from '../../../../ModifyRow/EditRowFormContainer';

interface EditRowDialogProps {
  source: Source;
  table: Table;
  row: Record<string, any>;
  onClose: () => void;
  /** called once the row is saved, e.g. to reload the rows */
  onSaved?: () => void;
}

export const EditRowDialog = ({
  source,
  table,
  row,
  onClose,
  onSaved,
}: EditRowDialogProps) => (
  <Dialog title="Edit Row" onClose={onClose} size="xxxl">
    <EditRowFormContainer
      source={source}
      table={table}
      row={row}
      onSuccess={() => {
        onSaved?.();
        onClose();
      }}
    />
  </Dialog>
);
