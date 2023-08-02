import startCase from 'lodash/startCase';
import { Breadcrumbs } from '../../../../new-components/Breadcrumbs';
import { StoredProcedureWidget } from './StoredProcedureWidget';
import { InjectedRouter, withRouter } from 'react-router';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';

export const TrackStoredProcedureRoute = withRouter<{
  location: Location;
  router: InjectedRouter;
}>(({ location, router }) => {
  const pathname = location.pathname;
  const push = router.push;
  const paths = pathname?.split('/').filter(Boolean) ?? [];
  return (
    <div className="flex flex-col">
      <div className="py-md px-md w-full">
        <Breadcrumbs
          items={paths.map((path: string, index) => {
            return {
              title: startCase(path),
              onClick:
                index === paths.length - 1
                  ? undefined
                  : () => {
                      push?.(`/${paths.slice(0, index + 1).join('/')}`);
                    },
            };
          })}
        />
        <div className="w-full">
          <div className="text-xl font-bold mt-2">Track Stored Procedure</div>
          <div className="text-muted">
            Expose your stored SQL procedures via the GraphQL API.{' '}
            <LearnMoreLink href="https://hasura.io/docs/latest/schema/ms-sql-server/logical-models/stored-procedures/#step-2-track-a-stored-procedure" />
          </div>
        </div>
      </div>
      <div className="px-md w-full flex flex-col">
        <StoredProcedureWidget />
      </div>
    </div>
  );
});
