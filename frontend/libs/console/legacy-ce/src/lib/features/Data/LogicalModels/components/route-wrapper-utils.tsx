import startCase from 'lodash/startCase';
import { BreadcrumbItem } from '../../../../new-components/Breadcrumbs/Breadcrumbs';
import { RouteWrapperProps } from './RouteWrapper';

export const injectRouteDetails = (
  path: string,
  {
    itemName,
    itemSourceName,
    itemTabName,
  }: Pick<RouteWrapperProps, 'itemName' | 'itemSourceName' | 'itemTabName'>
) => {
  return path
    .replace('{{source}}', itemSourceName ?? '')
    .replace('{{name}}', itemName ?? '')
    .replace('{{tab}}', itemTabName ?? '');
};

export const pathsToBreadcrumbs = (
  paths: string[],
  props: RouteWrapperProps,
  _push: (path: string) => void
): BreadcrumbItem[] =>
  paths.reduce<BreadcrumbItem[]>((prev, path, index, arr) => {
    // skip source in path
    if (path === '{{source}}') return prev;

    let title = injectRouteDetails(path, props);

    //prepend source to the item directly following the source so the source is visible in the breadcrumb but not it's own entry in the crumbs
    if (arr[index - 1] === '{{source}}') {
      const source = injectRouteDetails(arr[index - 1], props);
      title = `${source} / ${title}`;
    } else {
      title = startCase(title);
    }

    return [
      ...prev,
      {
        title,
        onClick:
          index === paths.length - 1
            ? undefined
            : () => {
                const pathIndex = paths.indexOf(path);

                const newPath = injectRouteDetails(
                  `/${paths.slice(0, pathIndex + 1).join('/')}`,
                  props
                );

                _push(newPath);
              },
      },
    ];
  }, []);
