import startCase from 'lodash/startCase';
import { Flex } from '@radix-ui/themes';
import { Breadcrumbs, LearnMoreLink } from '@hasura/shared/ui';
import { StoredProcedureWidget } from './StoredProcedureWidget';
import { useLocation, useNavigate } from 'react-router';

export const TrackStoredProcedureRoute = () => {
  const location = useLocation();
  const navigate = useNavigate();
  const pathname = location.pathname;
  const push = navigate;
  const paths = pathname?.split('/').filter(Boolean) ?? [];
  return (
    <Flex direction="column">
      <div className="py-4 px-4 w-full">
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
      <Flex direction="column" className="px-4 w-full">
        <StoredProcedureWidget />
      </Flex>
    </Flex>
  );
};
