import { CardedTable } from '../../../../new-components/CardedTable';
import { Button } from '../../../../new-components/Button';
import { FaEdit, FaTrash, FaUndo } from 'react-icons/fa';
import { useMetadata } from '../../../MetadataAPI';
import _push from '../../../../components/Services/Data/push';
import { useAppDispatch } from '../../../../store';
import { useReloadSource } from '../../hooks/useReloadSource';
import { useDropSource } from '../../hooks/useDropSource';

export const ListConnectedDatabases = () => {
  const dispatch = useAppDispatch();
  const { reloadSource, isLoading: isSourceReloading } = useReloadSource();
  const { dropSource, isLoading: isSourceRemovalInProgress } = useDropSource();

  const { data: databaseList, isLoading } = useMetadata(m =>
    m.metadata.sources.map(source => ({
      dataSourceName: source.name,
      driver: source.kind,
    }))
  );

  if (isLoading) return <>Loading...</>;

  const columns = ['database', 'driver'];

  const rowData = (databaseList ?? []).map(databaseItem => [
    <a href="!#" className="text-secondary">
      {databaseItem.dataSourceName}
    </a>,
    databaseItem.driver,
    <div className="flex gap-4 justify-end px-4">
      <Button
        icon={<FaUndo />}
        size="sm"
        onClick={() => reloadSource(databaseItem.dataSourceName)}
        isLoading={isSourceReloading}
        loadingText="Reloading"
      >
        Reload
      </Button>
      <Button
        icon={<FaEdit />}
        size="sm"
        onClick={() => {
          dispatch(
            _push(
              `/data/v2/database/edit?database=${databaseItem.dataSourceName}`
            )
          );
        }}
      >
        Edit
      </Button>
      <Button
        icon={<FaTrash />}
        mode="destructive"
        size="sm"
        onClick={() => {
          dropSource(databaseItem.driver, databaseItem.dataSourceName);
        }}
        isLoading={isSourceRemovalInProgress}
        loadingText="Deleting"
      >
        Remove
      </Button>
    </div>,
  ]);

  return (
    <div>
      <CardedTable columns={[...columns, null]} data={rowData} showActionCell />
    </div>
  );
};
