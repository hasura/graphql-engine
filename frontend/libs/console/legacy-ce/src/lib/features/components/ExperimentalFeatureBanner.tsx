import clsx from 'clsx';
import React from 'react';
import { FaGithub } from 'react-icons/fa';
import { Button } from '../../new-components/Button';
import { IndicatorCard } from '../../new-components/IndicatorCard';

const twButtonExperimental = `from-purple-50 to-purple-50 border-purple-300 hover:border-purple-500 focus-visible:from-purple-200 focus-visible:to-purple-200 disabled:border-purple-300 !text-purple-800`;

export const ExperimentalFeatureBanner: React.VFC<{
  githubIssueLink: string;
  feedbackIcon?: JSX.Element;
}> = ({ githubIssueLink, feedbackIcon }) => {
  return (
    <IndicatorCard
      status="experimental"
      className="py-4 px-md"
      showIcon
      contentFullWidth
    >
      <div className='flex items-center justify-between mx-4"'>
        <div>
          <h1 className="text-purple-800 font-bold text-lg">
            This is an experimental feature
          </h1>
          <div className="text-muted">
            Join the discussion on GitHub to talk about this feature or report
            bugs
          </div>
        </div>
        <a href={githubIssueLink} target="_blank" rel="noreferrer">
          <Button
            className={clsx(twButtonExperimental)}
            icon={feedbackIcon ?? <FaGithub />}
          >
            Share Feedback
          </Button>
        </a>
      </div>
    </IndicatorCard>
  );
};
