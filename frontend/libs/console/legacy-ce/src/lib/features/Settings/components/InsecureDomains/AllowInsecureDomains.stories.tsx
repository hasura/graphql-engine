import { StoryObj, Meta } from '@storybook/react-webpack5';
import { http, HttpResponse, delay } from 'msw';
import { ReactQueryDecorator } from '@hasura/shared/testing';

import AllowInsecureDomains from './AllowInsecureDomains';

const baseUrl = 'http://localhost:8080';

const mockHandlers = (hasDomains: boolean) => [
  http.post(`${baseUrl}/v1/metadata`, async () => {
    await delay(1);
    return HttpResponse.json({
      metadata: {
        version: 3,
        sources: [],
        inherited_roles: [],
        network: hasDomains
          ? {
              tls_allowlist: [
                { host: 'self-signed.example.com', suffix: '443' },
                { host: 'internal.example.com' },
              ],
            }
          : {},
      },
    });
  }),
];

export default {
  title: 'Features/Settings/AllowInsecureDomains',
  component: AllowInsecureDomains,
  parameters: {
    docs: { disable: true },
  },
  decorators: [ReactQueryDecorator()],
} as Meta<typeof AllowInsecureDomains>;

export const WithDomains: StoryObj<typeof AllowInsecureDomains> = {
  name: '💠 Demo With Domains',
  parameters: {
    msw: mockHandlers(true),
  },
};

export const Empty: StoryObj<typeof AllowInsecureDomains> = {
  name: '💠 Demo Empty',
  parameters: {
    msw: mockHandlers(false),
  },
};
