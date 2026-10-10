import { Route } from 'react-router';
import Error from './Error/Error';
import Operations from './Operations/Operations';
import Usage from './Usage/Usage';
import Overview from './Overview';
import Metrics from './Metrics';
import { moduleName } from './constants';

const metricsRouter = () => {
  return (
    <Route path={moduleName} element={<Metrics />}>
      <Route index element={<Overview />} />
      <Route path="error" element={<Error />} />
      <Route path="operations" element={<Operations />} />
      <Route path="usage" element={<Usage />} />
    </Route>
  );
};

export default metricsRouter;
