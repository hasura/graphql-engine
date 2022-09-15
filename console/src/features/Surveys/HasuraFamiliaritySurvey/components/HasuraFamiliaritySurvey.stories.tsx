import React from 'react';
import { FaBookmark, FaHeart, FaStar, FaUser } from 'react-icons/fa';
import { ComponentMeta, Story } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { IconCardGroupItem } from '@/new-components/IconCardGroup';
import { HasuraFamiliaritySurvey } from './HasuraFamiliaritySurvey';

export default {
  title: 'features/Surveys/HasuraFamiliaritySurvey',
  component: HasuraFamiliaritySurvey,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof HasuraFamiliaritySurvey>;

const data: { question: string; options: IconCardGroupItem<string>[] } = {
  question: 'How familiar are you with Hasura?',
  options: [
    {
      value: '1',
      icon: <FaHeart className="text-yellow-500 text-xl" />,
      title: 'New User',
      body: `I'm completely new to Hasura`,
    },
    {
      value: '2',
      icon: <FaBookmark className="text-yellow-500 text-xl" />,
      title: 'Past User',
      body: `I've used Hasura before but not actively developing right now`,
    },
    {
      value: '3',
      icon: <FaUser className="text-yellow-500 text-xl" />,
      title: 'Recurring User',
      body: `I'm already using Hasura (CE/Cloud) weekly/monthly`,
    },
    {
      value: '4',
      icon: <FaStar className="text-yellow-500 text-xl" />,
      title: 'Active User',
      body: `I'm actively developing with Hasura (CE/Cloud) daily`,
    },
  ],
};

export const Base: Story = () => (
  <HasuraFamiliaritySurvey
    data={data}
    onSkip={() => {}}
    onOptionClick={() => {}}
  />
);
