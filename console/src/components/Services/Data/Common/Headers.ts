import { GetReduxState } from '../../../../types';

const dataHeaders = (getState: GetReduxState) => {
  return getState().tables.dataHeaders;
};

export default dataHeaders;
