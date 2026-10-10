import { StoryObj, Meta } from '@storybook/react-webpack5';
import { Routes, Route } from 'react-router';
import { http, HttpResponse, delay } from 'msw';
import { ReactQueryDecorator } from '@hasura/shared/testing';

import Container from './Container';

const baseUrl = 'http://localhost:8080';

const mockHandlers = [
  http.post(`${baseUrl}/v1/metadata`, async () => {
    await delay(1);
    return HttpResponse.json({
      metadata: { version: 3, sources: [], inherited_roles: [] },
    });
  }),
];

export default {
  title: 'Features/Settings/Container',
  component: Container,
  parameters: {
    docs: { disable: true },
  },
  decorators: [ReactQueryDecorator()],
} as Meta<typeof Container>;

export const Default: StoryObj<typeof Container> = {
  name: '💠 Demo Settings Container',
  render: () => (
    <Routes>
      <Route element={<Container />}>
        <Route
          index
          element={<div className="p-4">Settings page content</div>}
        />
      </Route>
    </Routes>
  ),
  parameters: {
    msw: mockHandlers,
  },
};
