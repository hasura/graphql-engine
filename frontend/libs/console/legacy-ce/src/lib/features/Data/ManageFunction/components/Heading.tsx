import { Badge } from '../../../../new-components/Badge';
import { Button } from '../../../../new-components/Button';
import { DropdownMenu } from '../../../../new-components/DropdownMenu';
import React from 'react';
import { FaChevronDown } from 'react-icons/fa';
import { QualifiedFunction } from '../../../hasura-metadata-types';
import { hasuraToast } from '../../../../new-components/Toasts';
import { useAppDispatch } from '../../../../storeHooks';
import { getRoute } from '../../../../utils/getDataRoute';
import _push from '../../../../components/Services/Data/push';
import { useTrackFunction } from '../../hooks/useTrackFunction';
import { DisplayToastErrorMessage } from '../../components/DisplayErrorMessage';
import { FunctionDisplayName } from '../../TrackResources/TrackFunctions/components/FunctionDisplayName';

export const Heading: React.VFC<{
  dataSourceName: string;
  qualifiedFunction: QualifiedFunction;
}> = ({ qualifiedFunction, dataSourceName }) => {
  const dispatch = useAppDispatch();
  const { untrackFunction } = useTrackFunction({
    dataSourceName,
    onSuccess: () => {
      hasuraToast({
        type: 'success',
        title: 'Successfully untracked function',
      });
      dispatch(_push(getRoute().database(dataSourceName)));
    },
    onError: err => {
      hasuraToast({
        type: 'error',
        title: 'Error while untracking table',
        children: <DisplayToastErrorMessage message={err.message} />,
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
                    untrackFunction({
                      functionsToBeUntracked: [qualifiedFunction],
                    });
                  }}
                >
                  Untrack
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
                  <FunctionDisplayName qualifiedFunction={qualifiedFunction} />
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
