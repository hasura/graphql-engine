import clsx from 'clsx';
import isEmpty from 'lodash/isEmpty';
import { useContext, useRef } from 'react';
import { PermissionsInput } from './PermissionsInput';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { Token } from './Token';

export const RootInput = () => {
  const ref = useRef<HTMLDivElement>(null);
  const { permissions, isLoading } = useContext(rowPermissionsContext);
  return (
    <div
      ref={ref}
      className={`p-6 rounded-lg bg-white border border-gray-200 w-full ${
        isLoading ? 'animate-pulse bg-gray-200' : ''
      }`}
      data-testid={isLoading ? 'RootInputLoading' : 'RootInputReady'}
    >
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
