import React from 'react';
import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { Button } from '../../../../new-components/Button';
import { eeLicenseInfo } from '../../mocks/http';
import { WithEEBenefits } from './WithEEBenefits';
import { useQueryClient } from 'react-query';
import { EE_LICENSE_INFO_QUERY_NAME } from '../../constants';

export default {
  title: 'features/EETrial/ BenefitsView 🧬️',
  parameters: {
    Benefits: {
      source: { type: 'code' },
    },
  },
  component: WithEEBenefits,
  decorators: [
    // This is done so as we have set some cache time on the EE_LICENSE_INFO_QUERY_NAME query.
    // So we need to refetch the cache data, so it doesn't persist across different stories. And
    // it makes sure that our component actually does the network call, letting msw mocks return the
    // desired response.
    Story => {
      const queryClient = useQueryClient();
      void queryClient.refetchQueries(EE_LICENSE_INFO_QUERY_NAME);
      return <Story />;
    },
    ReactQueryDecorator(),
  ],
} as Meta<typeof WithEEBenefits>;

export const ButtonWithEEBenefits: StoryObj<typeof WithEEBenefits> = {
  render: args => (
    <div className="w-full h-20 flex items-center justify-center bg-slate-600">
      <WithEEBenefits id="button-with-ee-benefits">
        <Button mode="primary">Button With EE Benefits</Button>
      </WithEEBenefits>
    </div>
  ),

  parameters: {
    msw: [eeLicenseInfo.active],
  },
};
