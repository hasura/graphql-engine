import React from 'react';
import ReactJson from 'react-json-view';

import { Story, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { Button } from '@/new-components/Button';

import { handlers } from '../../mocks/handlers.mock';
import {
  useUpdatePermissions,
  UseUpdatePermissionsArgs,
} from './useUpdatePermissions';
import { useDefaultValues } from '..';

const UseUpdatePermissionsComponent = ({
  schemaName,
  tableName,
  roleName,
  queryType,
  accessType,
}: UseUpdatePermissionsArgs) => {
  const { data } = useDefaultValues({
    schemaName,
    tableName,
    roleName,
    queryType,
  });

  const { updatePermissions, deletePermissions } = useUpdatePermissions({
    schemaName,
    tableName,
    roleName,
    queryType,
    accessType,
  });

  return (
    <div>
      <p>
        <strong>Data to submit</strong>
      </p>
      {!!data && <ReactJson src={data} />}

      <div className="flex gap-2 mt-2">
        <Button
          mode="primary"
          isLoading={updatePermissions.isLoading}
          onClick={() => updatePermissions.submit(data!)}
        >
          Submit
        </Button>
        <Button
          mode="destructive"
          isLoading={deletePermissions.isLoading}
          onClick={() => deletePermissions.submit(['insert'])}
        >
          Delete
        </Button>
      </div>
      <div className="mt-2">
        {!!updatePermissions.data && <ReactJson src={updatePermissions.data} />}

        {!!deletePermissions.data && <ReactJson src={deletePermissions.data} />}
      </div>
    </div>
  );
};

export default {
  title: 'Features/Permissions Form/hooks/useUpdatePermissions',
  component: UseUpdatePermissionsComponent,
  decorators: [ReactQueryDecorator()],
  parameters: {
    msw: handlers,
    chromatic: { disableSnapshot: true },
  },
} as Meta;

const schemaName = 'public';
const tableName = 'users';
const roleName = 'user';

export const Primary: Story<UseUpdatePermissionsArgs> = args => (
  <UseUpdatePermissionsComponent {...args} />
);
Primary.args = {
  schemaName,
  tableName,
  roleName,
  queryType: 'insert',
  accessType: 'fullAccess',
};
