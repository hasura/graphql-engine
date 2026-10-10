import React from 'react';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { http, HttpResponse } from 'msw';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';

import EventSidebar from '.';

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      retry: false,
      gcTime: 0,
    },
  },
});

const baseUrl = 'http://localhost:8080';

const generateMetadata = (eventTriggerCount: number) => ({
  version: 3,
  sources: [
    {
      name: 'default',
      kind: 'postgres',
      tables: Array.from({ length: eventTriggerCount }, (_, i) => ({
        table: { schema: 'public', name: `table_${i}` },
        event_triggers: [
          {
            name: `event_trigger_${i.toString().padStart(5, '0')}`,
            definition: {
              enable_manual: false,
              insert: { columns: '*' },
            },
            retry_conf: {
              num_retries: 0,
              interval_sec: 10,
              timeout_sec: 60,
            },
            webhook: `https://example.com/webhook/${i}`,
          },
        ],
      })),
      configuration: {
        connection_info: {
          database_url: 'postgres://localhost:5432/postgres',
        },
      },
    },
    {
      name: 'secondary',
      kind: 'postgres',
      tables: Array.from({ length: eventTriggerCount }, (_, i) => ({
        table: { schema: 'public', name: `table_${i}` },
        event_triggers: [
          {
            name: `event_trigger_${i.toString().padStart(5, '0')}`,
            definition: {
              enable_manual: false,
              insert: { columns: '*' },
            },
            retry_conf: {
              num_retries: 0,
              interval_sec: 10,
              timeout_sec: 60,
            },
            webhook: `https://example.com/webhook/${i}`,
          },
        ],
      })),
      configuration: {
        connection_info: {
          database_url: 'postgres://localhost:5432/postgres',
        },
      },
    },
  ],
});

const mockHandlers = (eventTriggerCount: number) => [
  http.post(`${baseUrl}/v1/metadata`, async () => {
    return HttpResponse.json({ metadata: generateMetadata(eventTriggerCount) });
  }),
];

export default {
  title: 'Features/Eventing/EventSidebar',
  component: EventSidebar,
  parameters: {
    docs: { disable: true },
  },
  decorators: [
    (Story: React.FC) => (
      <div className="max-w-xs">
        <QueryClientProvider client={queryClient}>
          <Story />
        </QueryClientProvider>
      </div>
    ),
  ],
} as Meta<typeof EventSidebar>;

export const Default: StoryObj<typeof EventSidebar> = {
  name: '💠 Demo Default',
  args: {
    triggerName: 'event_trigger_00000',
  },
  parameters: {
    msw: mockHandlers(200),
  },
};

export const LargeNumberOfItemsPerformance: StoryObj<typeof EventSidebar> = {
  name: '⚡️ Performance - 2000 Event Triggers',
  args: {
    triggerName: undefined,
  },
  parameters: {
    msw: mockHandlers(1000),
  },
};
