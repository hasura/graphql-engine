import React from 'react';
import { TaskEvent } from '@/features/ConnectDB';
import { PGFunction } from '../../../../dataSources/services/postgresql/types';
import { LS_KEYS, getLSItem, setLSItem } from '../../../../utils/localStorage';
import globals from '../../../../Globals';

export const getTrackableFunctions = (
  functionsList: PGFunction[],
  trackedFunctions: PGFunction[]
): PGFunction[] => {
  const trackedFuncNames = trackedFunctions.map(fn => fn.function_name);
  const filterCondition = (func: PGFunction) =>
    !trackedFuncNames.includes(func.function_name);
  return functionsList.filter(filterCondition);
};

export const isTrackableAndComputedField = (func: PGFunction) => {
  return (
    func.return_type_type === 'c' &&
    (func.function_type === 'STABLE' || func.function_type === 'IMMUTABLE') &&
    func.returns_set
  );
};

export const useVPCBannerVisibility = () => {
  // find whether the banner was dismissed
  const lastDismissed = new Date(
    getLSItem(LS_KEYS.vpcBannerLastDismissed) || ''
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

export type DBLatencyData = TaskEvent['public_event_data']['sources'][number];

const isSourceLatencyGood = (sourceLatencyData: DBLatencyData) => {
  return sourceLatencyData.avg_latency <= 100 && sourceLatencyData.error === '';
};

export const checkHighLatencySources = (taskEvent: TaskEvent | undefined) => {
  if (!taskEvent) {
    return false;
  }
  return Object.keys(taskEvent.public_event_data.sources).every(sourceName =>
    isSourceLatencyGood(taskEvent.public_event_data.sources[sourceName])
  );
};

export const getSourceInfoFromLatencyData = (
  sourceName: string,
  latencyData?: TaskEvent
) => {
  if (!latencyData) {
    return undefined;
  }

  return latencyData.public_event_data.sources[sourceName] || undefined;
};
