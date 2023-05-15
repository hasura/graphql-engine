import { InjectedRouter, withRouter } from 'react-router';
import { AddNativeQuery } from './AddNativeQuery';

export const AddNativeQueryRoute = withRouter<{
  location: Location;
  router: InjectedRouter;
}>(({ location, router }) => {
  return <AddNativeQuery pathname={location.pathname} push={router.push} />;
});
