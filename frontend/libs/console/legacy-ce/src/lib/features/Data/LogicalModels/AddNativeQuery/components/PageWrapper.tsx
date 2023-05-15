import { Breadcrumbs } from '../../../../../new-components/Breadcrumbs';
import startCase from 'lodash/startCase';

export const PageWrapper: React.FC<{
  pathname: string | undefined;
  push?: (to: string) => void;
}> = ({ children, push, pathname }) => {
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
          <div className="text-xl font-bold mt-2">Create Native Query</div>
          <div className="text-muted">
            Access more queries and operators through SQL on your database.
          </div>
        </div>
      </div>
      <div className="px-md w-full flex flex-col">{children}</div>
    </div>
  );
};
