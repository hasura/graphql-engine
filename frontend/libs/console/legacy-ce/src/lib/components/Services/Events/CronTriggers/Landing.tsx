import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { Button } from '../../../../new-components/Button';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';
import globals from '../../../../Globals';
import { getAddSTRoute } from '../../../Common/utils/routesUtils';
import { mapDispatchToPropsEmpty } from '../../../Common/utils/reactUtils';
import { CRON_TRIGGER } from '../constants';
import TopicDescription from '../../Common/Landing/TopicDescription';
import _push from '../../Data/push';

type Props = InjectedProps;

const Landing: React.FC<Props> = props => {
  const { dispatch } = props;

  const topicDescription = (
    <div>
      {CRON_TRIGGER}s can be used to reliably trigger HTTP endpoints to run some
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
      <div className="pl-0 w-full mt-md bootstrap-jail">
        <div className="pl-md">
          <div className="flex">
            <h2 className="text-xl font-bold mr-md">{CRON_TRIGGER}s</h2>
            <div className="ml-md">
              <Button
                mode="primary"
                size="md"
                onClick={() => dispatch(_push(getAddSTRoute()))}
                data-test="create-cron-trigger"
              >
                Create
              </Button>
            </div>
          </div>
          <hr className="my-md" />
          <div>
            <TopicDescription
              title="What are Cron Triggers?"
              imgUrl={`${globals.assetsPath}/common/img/cron-trigger.png`}
              imgAlt={CRON_TRIGGER}
              description={topicDescription}
            />
            <hr className="clear-both my-lg" />
          </div>
        </div>
      </div>
    </Analytics>
  );
};

const connector = connect(null, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const LandingConnector = connector(Landing);
export default LandingConnector;
