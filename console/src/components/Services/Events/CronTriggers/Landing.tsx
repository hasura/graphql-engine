import React from 'react';
import { push } from 'react-router-redux';
import { connect, ConnectedProps } from 'react-redux';
import globals from '../../../../Globals';
import Button from '../../../Common/Button/Button';
import styles from '../Events.scss';
import { getAddSTRoute } from '../../../Common/utils/routesUtils';
import { mapDispatchToPropsEmpty } from '../../../Common/utils/reactUtils';
import { CRON_TRIGGER } from '../constants';
import TopicDescription from '../../Common/Landing/TopicDescription';

interface Props extends InjectedProps {}

const Landing: React.FC<Props> = props => {
  const { dispatch } = props;
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
            onClick={() => dispatch(push(getAddSTRoute()))}
          >
            Create
          </Button>
        </div>
        <hr />
        <div>
          <TopicDescription
            title="What are Cron Triggers?"
            imgUrl={`${globals.assetsPath}/common/img/event-trigger.png`}
            imgAlt={CRON_TRIGGER}
            description={`${CRON_TRIGGER}s are used to trigger HTTP endpoints based on a Cron Schedule.`}
          />
          <hr className={styles.clear_fix} />
        </div>
      </div>
    </div>
  );
};

const connector = connect(null, mapDispatchToPropsEmpty);

type InjectedProps = ConnectedProps<typeof connector>;

export default connector(Landing);
