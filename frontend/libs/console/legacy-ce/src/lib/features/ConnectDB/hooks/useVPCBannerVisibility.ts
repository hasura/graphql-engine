import React from 'react';
import { getLSItem, setLSItem } from '@hasura/shared/utils';
import globals from '../../../Globals';
import { LS_KEYS } from '@hasura/shared/types';

export const useVPCBannerVisibility = () => {
  // find whether the banner was dismissed
  const lastDismissed = new Date(
    getLSItem(LS_KEYS.vpcBannerLastDismissed) || '',
  );
  let isDissmised = false;
  if (lastDismissed.toString() !== 'Invalid Date') {
    if (lastDismissed.getTime() < new Date().getTime()) {
      isDissmised = true;
    }
  }

  // show banner only if the context is cloud and it was not dismissed
  const shouldShowBanner =
    !isDissmised && globals.consoleType === 'cloud' && !globals.eeMode;

  const [show, setShow] = React.useState(shouldShowBanner);

  // callback to dismiss the banner
  const dismiss = () => {
    setLSItem(LS_KEYS.vpcBannerLastDismissed, new Date().toString());
    setShow(false);
  };

  return {
    show,
    dismiss,
  };
};
