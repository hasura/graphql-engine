import isEmpty from 'lodash/isEmpty';
import { Entry } from './Entry';
import { Operator } from './Operator';
import { isPrimitive } from './utils';
import { ValueInput } from './ValueInput';
import { Permissions } from './types';

export const PermissionsInput = ({
  permissions, // permissions object, array, or primitive
  path, // array of strings that represents the path to the current permission in the permissions object
}: {
  permissions: Permissions;
  path: string[];
}) => {
  const currentPath = path[path.length - 1];
  const isInitialEmptyState = isEmpty(permissions) && path.length === 0;
  if (isInitialEmptyState) {
    return <Operator operator={'_eq'} path={[]} v={''} />;
  }
  if (isPrimitive(permissions) || currentPath === '_table') {
    return <ValueInput value={permissions} path={path} />;
  }
  if (Array.isArray(permissions)) {
    return (
      <>
        {permissions.map((v, i) => {
          const index = isEmpty(v) ? 0 : i;
          // Don't show empty arrays. They get handled by <EmptyEntry/ >
          if (v === '') return null;
          return (
            <PermissionsInput
              key={path.join('.') + '.' + index}
              permissions={v}
              path={[...path, `${index}`]}
            />
          );
        })}
      </>
    );
  }
  const entries = Object.entries(permissions);
  return (
    <>
      {entries.map(([k, v]) => {
        const childPath = [...path, k];
        return <Entry key={k} path={childPath} k={k} v={v} />;
      })}
    </>
  );
};
