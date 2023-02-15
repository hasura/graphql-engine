import { isEmpty } from 'lodash';
import { Entry } from './Entry';
import { Operator } from './Operator';
import { isPrimitive } from './utils';
import { ValueInput } from './ValueInput';
import { Permissions } from './types';

export const PermissionsInput = ({
  permissions,
  path,
}: {
  permissions: Permissions;
  path: string[];
}) => {
  const currentPath = path[path.length - 1];
  if (isEmpty(permissions) && path.length === 0) {
    return <Operator operator={'_eq'} path={[]} />;
  }
  if (isPrimitive(permissions) || currentPath === '_table') {
    return <ValueInput value={permissions} path={path} />;
  }
  if (Array.isArray(permissions)) {
    return (
      <>
        {permissions.map((v, i) => {
          const index = isEmpty(v) ? 0 : i;
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
