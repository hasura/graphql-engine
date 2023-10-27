import { InjectedRouter, withRouter } from 'react-router';
import { RouteWrapper } from '../components/RouteWrapper';
import { AddNativeQuery } from './AddNativeQuery';
import { Routes } from '../constants';

export const AddNativeQueryRoute = withRouter<{
  location: Location;
  router: InjectedRouter;
}>(({ location, router }) => {
  return (
    <RouteWrapper route={Routes.CreateNativeQuery}>
      <AddNativeQuery />
    </RouteWrapper>
  );
});
