import { GetReduxState } from '../../../../Types';

const dataHeaders = (getState: GetReduxState) => {
  return getState().tables.dataHeaders;
};

export default dataHeaders;
