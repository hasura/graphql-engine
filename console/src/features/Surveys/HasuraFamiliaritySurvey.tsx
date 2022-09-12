import React from 'react';
import { FaStar, FaHeart, FaUser, FaBookmark } from 'react-icons/fa';
import { Dialog } from '@/new-components/Dialog';
import { IconCardGroup } from '@/new-components/IconCardGroup';

type CustomDialogFooterProps = {
  onSkip: () => void;
};

const CustomDialogFooter: React.FC<CustomDialogFooterProps> = props => {
  const { onSkip } = props;

  return (
    <div className="flex justify-center border-t border-gray-300 bg-white p-sm">
      <div className="flex">
        <div className="ml-2">
          <a
            className="underline text-blue-600 cursor-pointer"
            onClick={onSkip}
          >
            Skip
          </a>
        </div>
      </div>
    </div>
  );
};

export type Props = {
  // onSubmit: () => void;
  onSkip: () => void;
};

// TODO: Subscribe this data object to db data using a custom hook for data fetching.
const data: {
  value: string;
  icon: React.ReactNode;
  title: string;
  body: string;
}[] = [
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
];

export const Root: React.FC<Props> = ({ onSkip }) => {
  return (
    <Dialog
      title="Welcome To Hasura!"
      description={`We'd love to get to know you before you get started with your first API.`}
      hasBackdrop
      footer={<CustomDialogFooter onSkip={onSkip} />}
    >
      <div className="mx-2 px-2">
        <div className="font-bold">How familiar are you with Hasura?</div>
        <div className="flex justify-center">
          <IconCardGroup items={data} disabled={false} onChange={() => {}} />
        </div>
      </div>
    </Dialog>
  );
};
