import React from 'react';
import { ComponentMeta } from '@storybook/react';
import VPCBanner from './VPCBanner';

export default {
  title: 'components/VPCBanner',
  component: VPCBanner,
  parameters: {
    layout: 'centered',
  },
} as ComponentMeta<typeof VPCBanner>;

export const Showcase = () => (
  <VPCBanner onClose={() => window.alert('Close banner clicked')} />
);
