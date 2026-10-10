import { Button, IndicatorCard, LearnMoreLink } from '@hasura/shared/ui';
import { FaRedoAlt, FaExternalLinkAlt } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';

import React from 'react';

export function AccelerateProject({
  isLoading,
  onReCheckClick,
  onUpdateRegionClick,
}: {
  isLoading: boolean;
  onReCheckClick: () => void;
  onUpdateRegionClick: () => void;
}) {
  return (
    <div className="mt-2">
      <IndicatorCard
        status="negative"
        headline="Accelerate your Hasura Project"
      >
        <Flex align="center" direction="row">
          <span>
            Databases marked with “Elevated Latency” indicate that it took us
            over 200 ms for this Hasura project to communicate with your
            database. These conditions generally happen when databases and
            projects are in geographically distant regions. This can cause API
            and subsequently application performance issues. We want your
            GraphQL APIs to be <b>lightning fast</b>, therefore we recommend
            that you either deploy your Hasura project in the same region as
            your database or select a database instance that&apos;s closer to
            where you&apos;ve deployed Hasura.
            <LearnMoreLink href="https://hasura.io/docs/latest/projects/regions/#changing-region-of-an-existing-project" />
          </span>
          <Flex align="center" direction="row" className="ml-xs">
            <Button
              className="mr-1"
              onClick={onReCheckClick}
              loading={isLoading}
              loadingText="Measuring Latencies..."
              leftIcon={FaRedoAlt}
            >
              Re-check Database Latency
            </Button>
            <Button
              className="mr-1"
              onClick={onUpdateRegionClick}
              leftIcon={FaExternalLinkAlt}
            >
              Update Project Region
            </Button>
          </Flex>
        </Flex>
      </IndicatorCard>
    </div>
  );
}
