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

const dataLeaf = {
  type: 'schema',
  name: 'users',
  leaf: {
    type: 'table',
    name: 'users',
  },
};

const UseUpdatePermissionsComponent = ({
  dataTarget,
  roleName,
  queryType,
  accessType,
}: UseUpdatePermissionsArgs) => {
  const { data } = useDefaultValues({
    dataTarget,
    roleName,
    queryType,
  });

  const { updatePermissions, deletePermissions } = useUpdatePermissions({
    dataTarget,
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

const roleName = 'user';

export const Primary: Story<UseUpdatePermissionsArgs> = args => (
  <UseUpdatePermissionsComponent {...args} />
);
Primary.args = {
  dataTarget: {
    dataSource: {
      driver: 'postgres',
      database: 'default',
    },
    dataLeaf,
  },
  roleName,
  queryType: 'insert',
  accessType: 'fullAccess',
};
