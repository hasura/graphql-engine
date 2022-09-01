import { Dispatch } from '../../../types';

export const getReactHelmetTitle = (feature: string, service: string) => {
  return `${feature} - ${service} | Hasura`;
};

/*
 * called "mapDispatchToPropsEmpty" because it just maps
 * the "dispatch" function and not any custom dispatchers
 */
export const mapDispatchToPropsEmpty = (dispatch: Dispatch) => ({ dispatch });
