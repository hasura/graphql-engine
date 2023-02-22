import { LS_KEYS, getLSItem } from '../../../../utils/localStorage';

export const isPermissionModalDisabled = () =>
  getLSItem(LS_KEYS.permissionConfirmationModalStatus) === 'disabled';
