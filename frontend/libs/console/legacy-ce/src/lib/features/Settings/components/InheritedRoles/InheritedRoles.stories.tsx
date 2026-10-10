import { StoryObj, Meta } from '@storybook/react-webpack5';
import { http, HttpResponse, delay } from 'msw';
import { ReactQueryDecorator } from '@hasura/shared/testing';

import InheritedRoles from './InheritedRoles';

const baseUrl = 'http://localhost:8080';

const mockHandlers = [
  http.post(`${baseUrl}/v1/metadata`, async ({ request }) => {
    const body = (await request.json()) as { type: string };
    await delay(1);

    if (body.type === 'get_inconsistent_metadata') {
      return HttpResponse.json({
        inconsistent_objects: [],
        is_consistent: true,
      });
    }

    return HttpResponse.json({
      metadata: {
        version: 3,
        sources: [],
        inherited_roles: [
          { role_name: 'manager', role_set: ['user', 'editor'] },
          { role_name: 'auditor', role_set: ['user'] },
        ],
      },
    });
  }),
];

export default {
  title: 'Features/Settings/InheritedRoles',
  component: InheritedRoles,
  parameters: {
    docs: { disable: true },
  },
  decorators: [ReactQueryDecorator()],
} as Meta<typeof InheritedRoles>;

export const Default: StoryObj<typeof InheritedRoles> = {
  name: '💠 Demo Inherited Roles',
  parameters: {
    msw: mockHandlers,
  },
};
