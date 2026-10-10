import React from 'react';
import { StoryObj, Meta } from '@storybook/react-webpack5';
import { Flex } from '@radix-ui/themes';
import { ReactQueryDecorator } from '@hasura/shared/testing';
import { Button } from '@hasura/shared/ui';
import { eeLicenseInfo } from '../../mocks/http';
import { WithEEBenefits } from './WithEEBenefits';
import { useQueryClient } from '@tanstack/react-query';
import { EE_LICENSE_INFO_QUERY_NAME } from '../../constants';

export default {
  title: 'features/EETrial/WithEEBenefits 🧬️',
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
    (Story) => {
      const queryClient = useQueryClient();
      void queryClient.refetchQueries({ queryKey: EE_LICENSE_INFO_QUERY_NAME });
      return <Story />;
    },
    ReactQueryDecorator(),
  ],
} as Meta<typeof WithEEBenefits>;

export const ButtonWithEEBenefits: StoryObj<typeof WithEEBenefits> = {
  render: (args) => (
    <Flex align="center" justify="center" className="w-full h-20 bg-slate-600">
      <WithEEBenefits id="button-with-ee-benefits">
        <Button mode="primary">Button With EE Benefits</Button>
      </WithEEBenefits>
    </Flex>
  ),

  parameters: {
    msw: [eeLicenseInfo.active],
  },
};
