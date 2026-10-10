import React from 'react';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';

import { EETrialCard } from './EETrialCard';
import { Flex } from '@radix-ui/themes';

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      retry: false,
      gcTime: 0,
    },
  },
});

export default {
  title: 'features/EETrial/ EETrialCard 🧬',
  parameters: {
    docs: {
      description: {
        component: `A card displaying advantages of pro console.<br>
Default CSS display is \`block\`, provided without padding and margin (displayed here with \`padding: 1rem;\`)`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [
    (Story) => {
      window.localStorage.getItem = () => {
        return JSON.stringify([
          {
            enabled: true,
            dismissed: false,
            enableDate: '2022-11-23T16:50:45.080Z',
            id: 'f996c937-7935-4f2f-8120-b06eab0e39b9',
          },
        ]);
      };
      return (
        <QueryClientProvider client={queryClient}>
          <Flex
            direction="column"
            gap="5"
            align="center"
            className="p-4 max-w-screen"
          >
            {Story()}
          </Flex>
        </QueryClientProvider>
      );
    },
  ],
  component: EETrialCard,
} as Meta<typeof EETrialCard>;

export const ApiPlayground: StoryObj<typeof EETrialCard> = {
  name: '⚙️ API',

  args: {
    cardTitle: 'title',
    cardText: 'text',
    buttonLabel: 'buttonLabel',
  },
};

export const Basic: StoryObj<typeof EETrialCard> = {
  render: () => (
    <EETrialCard
      cardTitle="The card title"
      cardText="The card text"
      buttonLabel="The card button label"
    />
  ),

  name: '🧰 Basic',
};

export const TrialExpired: StoryObj<typeof EETrialCard> = {
  render: () => (
    <EETrialCard
      cardTitle="The card title"
      cardText="The card text"
      buttonLabel="The card button label"
      horizontal
      eeAccess="expired"
    />
  ),

  name: '🧰 Trial Expired',
};

export const TrialDeactivated: StoryObj<typeof EETrialCard> = {
  render: () => (
    <EETrialCard
      cardTitle="The card title"
      cardText="The card text"
      buttonLabel="The card button label"
      horizontal
      eeAccess="deactivated"
    />
  ),

  name: '🧰 Trial Deactivated',
};

export const VariantHorizontal: StoryObj<typeof EETrialCard> = {
  render: () => (
    <>
      <EETrialCard
        cardTitle="The card title"
        cardText="The card text"
        buttonLabel="The card button label"
        horizontal
      />
      <EETrialCard
        className="w-full"
        cardTitle="The card title"
        cardText="The card text"
        buttonLabel="The card button label"
        horizontal
      />
    </>
  ),

  name: '🎭 Variant - Horizontal',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const VariantDisabled: StoryObj<typeof EETrialCard> = {
  render: () => (
    <EETrialCard
      cardTitle="The card title"
      cardText="The card text"
      buttonLabel="The card button label"
      horizontal
      eeAccess="expired"
    />
  ),

  name: '🎭 Variant - Disabled',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const ForPrometheus: StoryObj<typeof EETrialCard> = {
  render: () => (
    <>
      <EETrialCard
        cardTitle="Looking for production observability and metrics?"
        cardText={
          <span>
            Get production-ready today with a <strong>30-day free trial</strong>{' '}
            of Hasura EE, no credit card required.
          </span>
        }
        buttonLabel="Get Started with EE"
        horizontal
        className="w-full"
      />
      <EETrialCard
        cardTitle="Looking for production observability and metrics?"
        cardText={
          <span>
            Get production-ready today with a <strong>30-day free trial</strong>{' '}
            of Hasura EE, no credit card required.
          </span>
        }
        buttonLabel="Get Started with EE"
        horizontal
      />
    </>
  ),

  name: '💠 Demo for Prometheus',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};

export const ForAllowList: StoryObj<typeof EETrialCard> = {
  render: () => (
    <>
      <EETrialCard
        cardTitle="Looking to enhance your allow list capabilities?"
        cardText="Unlock managing additional collections and granular role-based allow lists.."
        buttonLabel="Get Started with Hasura EE"
        buttonType="default"
      />
      <EETrialCard
        cardTitle="Looking to enhance your allow list capabilities?"
        cardText="Unlock managing additional collections and granular role-based allow lists."
        buttonLabel="Get Started with Hasura EE"
        buttonType="default"
      />
    </>
  ),

  name: '💠 Demo for Allow List',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};
