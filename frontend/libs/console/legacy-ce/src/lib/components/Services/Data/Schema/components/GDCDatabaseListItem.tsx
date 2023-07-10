import React from 'react';
import { FaExclamationTriangle } from 'react-icons/fa';
import { Source } from '../../../../../features/hasura-metadata-types';
import { exportMetadata } from '../../../../../metadata/actions';
import { Button } from '../../../../../new-components/Button';
import { Tooltip } from '../../../../../new-components/Tooltip';
import { Dispatch } from '../../../../../types';
import _push from '../../push';
import { isInconsistentSource } from '../../utils';
import { useDropSource } from '../hooks/useDropSource';
import { useReloadSource } from '../hooks/useReloadSource';

type GDCDatabaseListItemItemProps = {
  dataSource: Pick<Source, 'name' | 'kind'>;
  inconsistentObjects: Record<string, any>;
  dispatch: Dispatch;
};

// This appears to be dead code that's not referenced or implemented anywhere:
export const GDCDatabaseListItem: React.FC<GDCDatabaseListItemItemProps> = ({
  dataSource,
  inconsistentObjects,
  dispatch,
}) => {
  const { dropSource, isLoading: isDropSourceInProgress } = useDropSource({
    customOnSuccess: () => {
      dispatch(exportMetadata());
    },
  });

  const { reloadSource, isLoading: isReloadSourceInProgress } =
    useReloadSource();

  const isInconsistentDataSource = isInconsistentSource(
    dataSource.name,
    inconsistentObjects
  );

  return (
    <tr data-test={dataSource.name}>
      <td className="px-sm py-xs align-top w-0 whitespace-nowrap">
        <Button
          size="sm"
          className="mr-xs"
          isLoading={isDropSourceInProgress}
          onClick={() => {
            dispatch(
              _push(`/data/v2/manage/database?database=${dataSource.name}`)
            );
          }}
          disabled={isInconsistentDataSource}
        >
          View Database
        </Button>
        <Button
          size="sm"
          className="mr-xs"
          loadingText="Reloading..."
          isLoading={isReloadSourceInProgress}
          onClick={() => {
            reloadSource(dataSource.name);
          }}
        >
          Reload
        </Button>
        <Button
          size="sm"
          className="mr-xs"
          onClick={() => {
            dispatch(_push(`/data/v2/edit?database=${dataSource.name}`));
          }}
        >
          Edit
        </Button>
        <Button
          size="sm"
          loadingText="Removing..."
          className="text-red-600"
          onClick={() => {
            dropSource(dataSource.kind, dataSource.name);
          }}
        >
          Remove
        </Button>
      </td>
      <td className="px-sm py-xs max-w-xs align-center break-all">
        <div className="font-bold">
          {dataSource.name}{' '}
          <span className="font-normal">({dataSource.kind})</span>
        </div>
      </td>
      <td className="px-sm py-xs max-w-xs align-top">
        {isInconsistentDataSource && (
          <Tooltip tooltipContentChildren="Source is inconsistent">
            <FaExclamationTriangle
              className="ml-xs text-red-800"
              aria-hidden="true"
            />
          </Tooltip>
        )}
      </td>
      {/* <td className="px-sm py-xs max-w-xs align-top break-all">
                
      </td> */}
    </tr>
  );
};
