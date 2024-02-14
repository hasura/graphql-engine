import { useMutation } from 'react-query';
import {
  controlPlaneClient,
  updateUserClickedChangeProjectRegion,
} from '../../ControlPlane';

export const useUpdateProjectRegion = () => {
  return useMutation({
    mutationFn: async (rowId: string) => {
      if (!rowId) {
        return;
      }

      return controlPlaneClient.query(updateUserClickedChangeProjectRegion, {
        rowId,
        isChangeRegionClicked: true,
      });
    },
  });
};
