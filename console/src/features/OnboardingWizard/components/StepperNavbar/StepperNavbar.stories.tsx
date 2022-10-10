import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { StepperNavbar } from './StepperNavbar';

export default {
  title: 'features/Onboarding Wizard/Stepper Navbar',
  component: StepperNavbar,
} as ComponentMeta<typeof StepperNavbar>;

export const Base: Story = () => <StepperNavbar activeIndex={1} />;
