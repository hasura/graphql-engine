import React from 'react';
import { ComponentStory, ComponentMeta } from '@storybook/react';

import { SuccessScreen } from './SuccessScreen';
import { Dialog } from '../../../../../new-components/Dialog';

export default {
  title: 'features / EETrial / Activate EE Form / Success Screen 🧬️',
  component: SuccessScreen,
} as ComponentMeta<typeof SuccessScreen>;

export const Demo: ComponentStory<typeof SuccessScreen> = () => {
  return (
    <Dialog size="sm" onClose={() => {}} hasBackdrop>
      <SuccessScreen />
    </Dialog>
  );
};
Demo.storyName = '💠 Demo';
