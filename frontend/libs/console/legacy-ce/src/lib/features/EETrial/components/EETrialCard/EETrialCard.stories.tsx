import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';
import { QueryClient, QueryClientProvider } from 'react-query';

import { EETrialCard } from './EETrialCard';

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      retry: false,
      cacheTime: 0,
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
    Story => {
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
          <div className="p-4 flex flex-col gap-5 items-center max-w-screen">
            {Story()}
          </div>
        </QueryClientProvider>
      );
    },
  ],
  component: EETrialCard,
} as ComponentMeta<typeof EETrialCard>;

export const ApiPlayground: ComponentStory<typeof EETrialCard> = args => (
  <EETrialCard {...args} />
);
ApiPlayground.storyName = '⚙️ API';
ApiPlayground.args = {
  cardTitle: 'title',
  cardText: 'text',
  buttonLabel: 'buttonLabel',
};

export const Basic: ComponentStory<typeof EETrialCard> = () => (
  <EETrialCard
    cardTitle="The card title"
    cardText="The card text"
    buttonLabel="The card button label"
  />
);
Basic.storyName = '🧰 Basic';

export const TrialExpired: ComponentStory<typeof EETrialCard> = () => (
  <EETrialCard
    cardTitle="The card title"
    cardText="The card text"
    buttonLabel="The card button label"
    horizontal
    eeAccess="expired"
  />
);
TrialExpired.storyName = '🧰 Trial Expired';

export const TrialDeactivated: ComponentStory<typeof EETrialCard> = () => (
  <EETrialCard
    cardTitle="The card title"
    cardText="The card text"
    buttonLabel="The card button label"
    horizontal
    eeAccess="deactivated"
  />
);
TrialDeactivated.storyName = '🧰 Trial Deactivated';

export const VariantHorizontal: ComponentStory<typeof EETrialCard> = () => (
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
);
VariantHorizontal.storyName = '🎭 Variant - Horizontal';
VariantHorizontal.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const VariantDisabled: ComponentStory<typeof EETrialCard> = () => (
  <EETrialCard
    cardTitle="The card title"
    cardText="The card text"
    buttonLabel="The card button label"
    horizontal
    eeAccess="expired"
  />
);
VariantDisabled.storyName = '🎭 Variant - Disabled';
VariantDisabled.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const ForPrometheus: ComponentStory<typeof EETrialCard> = () => (
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
);
ForPrometheus.storyName = '💠 Demo for Prometheus';
ForPrometheus.parameters = {
  docs: {
    source: { state: 'open' },
  },
};

export const ForAllowList: ComponentStory<typeof EETrialCard> = () => (
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
);
ForAllowList.storyName = '💠 Demo for Allow List';
ForAllowList.parameters = {
  docs: {
    source: { state: 'open' },
  },
};
