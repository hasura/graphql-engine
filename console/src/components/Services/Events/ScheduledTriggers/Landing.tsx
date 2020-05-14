import React from 'react';
import { push } from 'react-router-redux';
import Button from '../../../Common/Button/Button';
import styles from '../Events.scss';
import { getAddSTRoute } from '../../../Common/utils/routesUtils';
import { Dispatch } from '../../../../types';

type Props = {
  dispatch: Dispatch;
};

const Landing: React.FC<Props> = props => {
  const { dispatch } = props;
  return (
    <div
      className={`${styles.padd_left_remove} container-fluid ${styles.padd_top}`}
    >
      <div className={styles.padd_left}>
        <div className={styles.display_flex}>
          <h2 className={`${styles.headerText} ${styles.inline_block}`}>
            Scheduled Triggers
          </h2>
          <Button
            color="yellow"
            size="sm"
            onClick={() => dispatch(push(getAddSTRoute()))}
          >
            Create
          </Button>
        </div>
      </div>
    </div>
  );
};

export default (connect: any) => connect()(Landing);
