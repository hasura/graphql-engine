import React from 'react';
import { Button, TopicDescription } from '@hasura/shared/ui';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { dataRoutes } from '@hasura/shared/utils';
import { useNavigate } from 'react-router';
import { Flex } from '@radix-ui/themes';
import { CRON_TRIGGER } from '@hasura/shared/types';
import { useAppContext } from '@hasura/shared/context';

const ScheduledTriggerLanding: React.FC = () => {
  const { envVars } = useAppContext();
  const navigate = useNavigate();

  const topicDescription = (
    <div>
      Cron Triggers can be used to reliably trigger HTTP endpoints to run some
      custom business logic periodically based on a{' '}
      <a
        href="https://en.wikipedia.org/wiki/Cron"
        target="_blank"
        rel="noopener noreferrer"
      >
        cron schedule
      </a>
      .
    </div>
  );

  return (
    <Analytics name="ScheduledTriggerLanding" {...REDACT_EVERYTHING}>
      <div className="pl-0 w-full mt-4 bootstrap-jail">
        <div className="pl-md">
          <Flex>
            <h2 className="text-xl font-bold mr-4">{CRON_TRIGGER}s</h2>
            <div className="ml-4">
              <Button
                mode="primary"
                size="md"
                onClick={() => navigate(dataRoutes.getAddSTRoute())}
                data-test="create-cron-trigger"
              >
                Create
              </Button>
            </div>
          </Flex>
          <hr className="my-4" />
          <div>
            <TopicDescription
              title="What are Cron Triggers?"
              imgUrl={`${envVars.assetsPath}/common/img/cron-trigger.png`}
              imgAlt={CRON_TRIGGER}
              description={topicDescription}
            />
            <hr className="clear-both my-6" />
          </div>
        </div>
      </div>
    </Analytics>
  );
};

export default ScheduledTriggerLanding;
