import { updateReduxResourceVersion } from './store-change-handlers/currentReduxResourceVersion';
import { syncResourceVersions } from './store-change-handlers/syncResourceVersions';

export type OnStoreChangeProps = { currentStore: any; previousStore: any };

export const onStoreChange = (props: OnStoreChangeProps) => {
  // execute functions here....
  syncResourceVersions(props);
  updateReduxResourceVersion(props);
};
