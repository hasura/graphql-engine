import clsx from 'clsx';
import isEmpty from 'lodash/isEmpty';
import { useContext } from 'react';
import { PermissionsInput } from './PermissionsInput';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { Token } from './Token';

export const RootInput = () => {
  const { permissions } = useContext(rowPermissionsContext);
  return (
    <div className="p-6 rounded-lg bg-white border border-gray-200w-full">
      <Token token={'{'} />
      <div
        className={clsx(
          `py-2 border-dashed border-l border-gray-200 `,
          isEmpty(permissions) && 'pl-6'
        )}
        id="permissions-form-builder"
      >
        <PermissionsInput permissions={permissions} path={[]} />
      </div>
      <Token token={'}'} />
    </div>
  );
};
