import React from 'react';
import { IconCardGroupItem } from '@/new-components/IconCardGroup';
import { FaBookmark, FaHeart, FaStar, FaUser } from 'react-icons/fa';

export const familiaritySurveyOptionCode = [
  'new user',
  'past user',
  'recurring user',
  'active user',
] as const;

export type FamiliaritySurveyOptionCode = typeof familiaritySurveyOptionCode[number];

const familiaritySurveyOptionDetails: Record<
  FamiliaritySurveyOptionCode,
  Omit<IconCardGroupItem<string>, 'value'>
> = {
  'new user': {
    icon: <FaHeart className="text-yellow-500 text-xl" />,
    title: 'New User',
    body: `I'm completely new to Hasura`,
  },
  'past user': {
    icon: <FaBookmark className="text-yellow-500 text-xl" />,
    title: 'Past User',
    body: `I've used Hasura before but not actively developing right now`,
  },
  'recurring user': {
    icon: <FaUser className="text-yellow-500 text-xl" />,
    title: 'Recurring User',
    body: `I'm already using Hasura (CE/Cloud) weekly/monthly`,
  },
  'active user': {
    icon: <FaStar className="text-yellow-500 text-xl" />,
    title: 'Active User',
    body: `I'm actively developing with Hasura (CE/Cloud) daily`,
  },
};

export function getFamiliaritySurveyOptionDetails(
  optionId: string,
  optionLabel: FamiliaritySurveyOptionCode
): IconCardGroupItem<string> {
  return {
    ...familiaritySurveyOptionDetails[optionLabel],
    value: optionId,
  };
}
