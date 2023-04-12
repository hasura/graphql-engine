import { Button } from '../../../../../new-components/Button';
import { FaRedoAlt, FaExternalLinkAlt } from 'react-icons/fa';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import { LearnMoreLink } from '../../../../../new-components/LearnMoreLink';
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
    <div className="mt-xs">
      <IndicatorCard
        status="negative"
        headline="Accelerate your Hasura Project"
      >
        <div className="flex items-center flex-row">
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
          <div className="flex items-center flex-row ml-xs">
            <Button
              className="mr-xs"
              onClick={onReCheckClick}
              isLoading={isLoading}
              loadingText="Measuring Latencies..."
              icon={<FaRedoAlt />}
            >
              Re-check Database Latency
            </Button>
            <Button
              className="mr-xs"
              onClick={onUpdateRegionClick}
              icon={<FaExternalLinkAlt />}
            >
              Update Project Region
            </Button>
          </div>
        </div>
      </IndicatorCard>
    </div>
  );
}
