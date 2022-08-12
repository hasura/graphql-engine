import * as React from 'react';

import { ProgressState } from './types';
import styles from '../styles.scss';
import StatusIcon from './StatusIcon';

type Props = {
  state: ProgressState;
};

const DBCreationStatus: React.FC<Props> = ({ state }) => {
  return (
    <div className={styles.wd100Percent}>
      <div className="row">
        <div className="col-md-12">
          <div
            className={`${styles.wd100Percent} ${styles.display_flex} ${styles.flex_justify_center} ${styles.add_mar_top}`}
          >
            <p className={`small ${styles.noWrap}`}>Creating Heroku app</p>
            <p
              className={`small ${styles.noWrap}`}
              style={{ padding: '0 85px' }}
            >
              Installing Postgres
            </p>
            <p className={`small ${styles.noWrap}`}>Getting Database URL</p>
          </div>
        </div>
        <div className="col-md-12">
          <div
            className={`${styles.wd100Percent} ${styles.display_flex} ${styles.flex_justify_center} ${styles.add_mar_top_small}`}
          >
            <StatusIcon state={state['creating-app']} />
            <div
              className={`${styles.progressBar} ${
                state['creating-app'].status === 'success'
                  ? styles.progressBarComplete
                  : ''
              }`}
            />
            <StatusIcon state={state['installing-postgres']} />
            <div
              className={`${styles.progressBar} ${
                state['installing-postgres'].status === 'success'
                  ? styles.progressBarComplete
                  : ''
              }`}
            />
            <StatusIcon state={state['getting-config']} />
          </div>
        </div>
      </div>
    </div>
  );
};

export default DBCreationStatus;
