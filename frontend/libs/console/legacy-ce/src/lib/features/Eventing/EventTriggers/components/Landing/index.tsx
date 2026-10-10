import React from 'react';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import {
  Button,
  Separator,
  TopicDescription,
  TryItOut,
} from '@hasura/shared/ui';
import { dataRoutes } from '@hasura/shared/utils';
import { EVENTS_SERVICE_HEADING, EVENT_TRIGGER } from '@hasura/shared/types';
import { useNavigate } from 'react-router';
import { useDocumentTitle } from '@hasura/shared/hooks';
import { Flex, Heading } from '@radix-ui/themes';
import { useAppContext } from '@hasura/shared/context';

const EventTriggerLanding: React.FC = () => {
  useDocumentTitle(
    dataRoutes.getReactHelmetTitle(EVENT_TRIGGER, EVENTS_SERVICE_HEADING),
  );
  const navigate = useNavigate();
  const { envVars } = useAppContext();

  const queryDefinition = `mutation {
insert_user(objects: [{name: "testuser"}] ){
  affected_rows
}
}`;
  const getIntroSection = () => {
    return (
      <div>
        <TopicDescription
          title={`What are ${EVENT_TRIGGER}s?`}
          imgUrl={`${envVars.assetsPath}/common/img/event-trigger.png`}
          imgAlt={`${EVENT_TRIGGER}s`}
          description={`An ${EVENT_TRIGGER} atomically captures events (insert, update, delete) on a specified table and then reliably calls a HTTP webhook to run some custom business logic.`}
          learnMoreHref="https://hasura.io/docs/latest/graphql/core/event-triggers/index.html"
        />
        <Separator size="4" className="my-6" />
      </div>
    );
  };

  const handleClick = (e: React.MouseEvent<HTMLButtonElement>) => {
    e.preventDefault();
    navigate(dataRoutes.getAddETRoute());
  };

  const footerEvent = (
    <span>
      Head to the Events tab and see an event invoked under{' '}
      <span className="font-bold"> test-trigger</span>.
    </span>
  );

  return (
    <Analytics name="EventTriggersLanding" {...REDACT_EVERYTHING}>
      <div className="w-full pt-4">
        <div>
          <Flex align="center" gap="2">
            <Heading size="4">{EVENT_TRIGGER}s</Heading>
            <Button
              data-testid="data-create-trigger"
              mode="primary"
              type="submit"
              onClick={handleClick}
            >
              Create
            </Button>
          </Flex>
          <Separator size="4" className="my-4" />

          {getIntroSection()}

          <TryItOut
            service="eventTrigger"
            title="Steps to deploy an example Event Trigger to Glitch"
            queryDefinition={queryDefinition}
            footerDescription={footerEvent}
            glitchLink="https://glitch.com/edit/#!/hasura-sample-event-trigger"
            googleCloudLink="https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/event-triggers/google-cloud-functions/nodejs8"
            microsoftAzureLink="https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/event-triggers/azure-functions/nodejs"
            awsLink="https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/event-triggers/aws-lambda/nodejs8"
            adMoreLink="https://github.com/hasura/graphql-engine/tree/master/community/boilerplates/event-triggers/"
          />
        </div>
      </div>
    </Analytics>
  );
};

export default EventTriggerLanding;
