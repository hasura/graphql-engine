import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import globals from '../../../../Globals';
import Button from '../../../Common/Button/Button';
import styles from '../Events.scss';
import { getAddSTRoute } from '../../../Common/utils/routesUtils';
import { mapDispatchToPropsEmpty } from '../../../Common/utils/reactUtils';
import { CRON_TRIGGER } from '../constants';
import TopicDescription from '../../Common/Landing/TopicDescription';
import _push from '../../Data/push';

interface Props extends InjectedProps {}

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
    <div
      className={`${styles.padd_left_remove} container-fluid ${styles.padd_top}`}
    >
      <div className={styles.padd_left}>
        <div className={styles.display_flex}>
          <h2 className={`${styles.headerText} ${styles.inline_block}`}>
            {CRON_TRIGGER}s
          </h2>
          <Button
            color="yellow"
            size="sm"
            onClick={() => dispatch(_push(getAddSTRoute()))}
          >
            Create
          </Button>
        </div>
        <hr className="my-md" />
        <div>
          <TopicDescription
            title="What are Cron Triggers?"
            imgUrl={`${globals.assetsPath}/common/img/cron-trigger.png`}
            imgAlt={CRON_TRIGGER}
            description={topicDescription}
          />
          <hr className={`${styles.clear_fix} my-lg`} />
        </div>
      </div>
    </div>
  );
};

const connector = connect(null, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

const LandingConnector = connector(Landing);
export default LandingConnector;
