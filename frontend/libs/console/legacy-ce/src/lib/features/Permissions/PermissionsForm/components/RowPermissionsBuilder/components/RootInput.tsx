import clsx from 'clsx';
import isEmpty from 'lodash/isEmpty';
import { useContext, useRef } from 'react';
import { PermissionsInput } from './PermissionsInput';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { Token } from './Token';
import { Card } from '@radix-ui/themes';

export const RootInput = () => {
  const ref = useRef<HTMLDivElement>(null);
  const { permissions, isLoading } = useContext(rowPermissionsContext);

  return (
    <Card
      ref={ref}
      className={`p-6 w-full ${isLoading ? 'animate-pulse' : ''}`}
      data-testid={isLoading ? 'RootInputLoading' : 'RootInputReady'}
    >
      <Token token={'{'} />
      <div
        className={clsx(
          `border-dashed border-l border-gray-200`,
          isEmpty(permissions) && 'pl-6',
        )}
        id="permissions-form-builder"
      >
        <PermissionsInput permissions={permissions} path={[]} />
      </div>
      <Token token={'}'} />
    </Card>
  );
};
