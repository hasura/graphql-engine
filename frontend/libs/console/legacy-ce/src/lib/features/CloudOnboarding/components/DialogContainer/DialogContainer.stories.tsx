import React from 'react';
import { StoryFn, Meta } from '@storybook/react';
import { DialogContainer } from './DialogContainer';
import { dialogHeader, familiaritySurveySubHeader } from '../../constants';

export default {
  title: 'features/CloudOnboarding/Onboarding Wizard/Dialog Container',
  component: DialogContainer,
} as Meta<typeof DialogContainer>;

const ExampleChildren = () => {
  return <div>Hello world!</div>;
};

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
  <DialogContainer
    header={dialogHeader}
    subHeader={familiaritySurveySubHeader}
    showStepper
    stepperNavSteps={stepperNavSteps}
    activeIndex={1}
  >
    <ExampleChildren />
  </DialogContainer>
);
