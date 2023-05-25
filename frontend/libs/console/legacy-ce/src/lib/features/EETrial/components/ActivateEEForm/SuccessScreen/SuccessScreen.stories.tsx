import React from 'react';
import { StoryObj, Meta } from '@storybook/react';

import { SuccessScreen } from './SuccessScreen';
import { Dialog } from '../../../../../new-components/Dialog';

export default {
  title: 'features / EETrial / Activate EE Form / Success Screen 🧬️',
  component: SuccessScreen,
} as Meta<typeof SuccessScreen>;

export const Demo: StoryObj<typeof SuccessScreen> = {
  render: () => {
    return (
      <Dialog size="sm" onClose={() => {}} hasBackdrop>
        <SuccessScreen />
      </Dialog>
    );
  },

  name: '💠 Demo',
};
