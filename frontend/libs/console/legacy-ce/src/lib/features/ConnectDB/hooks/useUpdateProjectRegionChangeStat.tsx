import { useMutation } from 'react-query';
import {
  controlPlaneClient,
  updateUserClickedChangeProjectRegion,
} from '../../ControlPlane';
import { CheckDatabaseLatencyResponse } from './useCheckDatabaseLatency';

type UpdateDBLatencyTableResponse = {
  affected_rows: number;
  returning: {
    id: string;
    is_change_region_clicked: boolean;
  };
};

export const useUpdateProjectRegionChangeStat = () => {
  return useMutation({
    mutationFn: async (
      checkDatabaseLatencyResponse:
        | CheckDatabaseLatencyResponse
        | string
        | undefined
    ) => {
      if (
        !checkDatabaseLatencyResponse ||
        typeof checkDatabaseLatencyResponse === 'string' ||
        !checkDatabaseLatencyResponse.insertDbLatencyData
      ) {
        return;
      }

      return controlPlaneClient.query<UpdateDBLatencyTableResponse>(
        updateUserClickedChangeProjectRegion,
        {
          rowId:
            checkDatabaseLatencyResponse.insertDbLatencyData.data
              .insert_db_latency_one.id,
          isChangeRegionClicked: true,
        }
      );
    },
  });
};
