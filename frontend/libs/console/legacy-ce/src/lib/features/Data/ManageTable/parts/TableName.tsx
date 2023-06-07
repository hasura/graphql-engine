import { Badge } from '../../../../new-components/Badge';
import { Button } from '../../../../new-components/Button';
import { DropdownMenu } from '../../../../new-components/DropdownMenu';
import React from 'react';
import { FaChevronDown, FaTable } from 'react-icons/fa';
import { useUntrackTable } from '../hooks/useUntrackTable';
import { Table } from '../../../hasura-metadata-types';
import { hasuraToast } from '../../../../new-components/Toasts';
import { useAppDispatch } from '../../../../storeHooks';
import { getRoute } from '../../../../utils/getDataRoute';
import _push from '../../../../components/Services/Data/push';
import AceEditor from 'react-ace';

export const TableName: React.VFC<{
  dataSourceName: string;
  table: Table;
  tableName: string;
}> = ({ tableName, dataSourceName, table }) => {
  const dispatch = useAppDispatch();
  const { untrackTable } = useUntrackTable({
    onSuccess: () => {
      hasuraToast({
        type: 'success',
        title: 'Successfully untracked table',
      });
      dispatch(_push(getRoute().database(dataSourceName)));
    },
    onError: err => {
      hasuraToast({
        type: 'error',
        title: 'Error while untracking table',
        children: (
          <div className="overflow-hidden">
            <AceEditor
              theme="github"
              setOptions={{
                maxLines: Infinity,
                showGutter: false,
                useWorker: false,
              }}
              value={JSON.stringify(err)}
              readOnly
            />
          </div>
        ),
      });
    },
  });

  return (
    <div className="flex items-center gap-3 mb-3">
      <div className="group relative">
        <div>
          <DropdownMenu
            items={[
              [
                <span
                  className="py-xs text-red-600"
                  onClick={() => {
                    untrackTable({ dataSourceName, table });
                  }}
                >
                  Untrack {tableName}
                </span>,
              ],
            ]}
          >
            <div className="flex gap-0.5 items-center">
              <Button
                iconPosition="end"
                icon={
                  <FaChevronDown
                    size={12}
                    className="text-gray-400 text-sm transition-transform group-radix-state-open:rotate-180"
                  />
                }
              >
                <div className="flex flex-row items-center ">
                  <FaTable className="mr-1.5" size={12} />
                  <span className="text-lg">{tableName}</span>
                </div>
              </Button>
            </div>
          </DropdownMenu>
        </div>
      </div>
      <div>
        <Badge color="green">Tracked</Badge>
      </div>
    </div>
  );
};
