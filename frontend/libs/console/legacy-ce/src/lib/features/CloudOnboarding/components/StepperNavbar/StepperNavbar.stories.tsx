import React from 'react';
import { StoryFn, Meta } from '@storybook/react';
import { StepperNavbar } from './StepperNavbar';

export default {
  title: 'features/CloudOnboarding/Onboarding Wizard/Stepper Navbar',
  component: StepperNavbar,
} as Meta<typeof StepperNavbar>;

const stepperNavSteps = [
  {
    step: '01',
    text: 'Getting Started',
  },
  {
    step: '02',
    text: 'Connect Database',
  },
  {
    step: '03',
    text: 'Make Your First Query',
  },
  {
    step: '04',
    text: 'Run Api',
  },
  {
    step: '05',
    text: 'Connect metadata',
  },
];

export const Base: StoryFn = () => (
  <StepperNavbar activeIndex={1} steps={stepperNavSteps} />
);
