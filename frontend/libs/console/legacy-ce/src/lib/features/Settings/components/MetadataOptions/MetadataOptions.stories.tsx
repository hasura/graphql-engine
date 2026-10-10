import { StoryObj, Meta } from '@storybook/react-webpack5';
import { http, HttpResponse, delay } from 'msw';
import { ReactQueryDecorator } from '@hasura/shared/testing';

import MetadataOptions from './MetadataOptions';

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
  title: 'Features/Settings/MetadataOptions',
  component: MetadataOptions,
  parameters: {
    docs: { disable: true },
  },
  decorators: [ReactQueryDecorator()],
} as Meta<typeof MetadataOptions>;

export const Default: StoryObj<typeof MetadataOptions> = {
  name: '💠 Demo Metadata Options',
  parameters: {
    msw: mockHandlers,
  },
};
