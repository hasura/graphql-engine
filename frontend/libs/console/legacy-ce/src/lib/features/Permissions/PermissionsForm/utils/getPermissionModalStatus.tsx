import { getLSItem } from '@hasura/shared/utils';
import { LS_KEYS } from '@hasura/shared/types';

export const isPermissionModalDisabled = () =>
  getLSItem(LS_KEYS.permissionConfirmationModalStatus) === 'disabled';
