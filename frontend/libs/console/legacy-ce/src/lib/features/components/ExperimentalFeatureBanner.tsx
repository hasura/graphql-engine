import React from 'react';
import { FaGithub } from 'react-icons/fa';
import { IconType } from 'react-icons';
import { Flex } from '@radix-ui/themes';
import { Button, IndicatorCard, Text } from '@hasura/shared/ui';

export const ExperimentalFeatureBanner: React.FC<{
  githubIssueLink: string;
  feedbackIcon?: IconType;
}> = ({ githubIssueLink, feedbackIcon }) => {
  return (
    <IndicatorCard
      status="experimental"
      showIcon
      headline="This is an experimental feature"
    >
      <Flex align="center" justify="between" gap="4">
        <Text as="div">
          Join the discussion on GitHub to talk about this feature or report
          bugs
        </Text>
        <div>
          <a href={githubIssueLink} target="_blank" rel="noreferrer">
            <Button color="purple" leftIcon={feedbackIcon ?? FaGithub}>
              Share Feedback
            </Button>
          </a>
        </div>
      </Flex>
    </IndicatorCard>
  );
};
