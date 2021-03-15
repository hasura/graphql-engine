import * as React from 'react';
import styles from '../styles.scss';
import Button from '../../../../../Common/Button';
import { HerokuSession } from './types';
import { Dispatch } from '../../../../../../types';
import LoginButton from './LoginButton';

import HerokuLogoComponent from './HerokuButtonLogo';

type Props = {
  session?: HerokuSession;
  startCreation: VoidFunction;
  dispatch: Dispatch;
};

const Intro: React.FC<Props> = ({ session, startCreation, dispatch }) => {
  return (
    <>
      <h4
        className={`${styles.remove_pad_bottom} ${styles.connect_db_header} ${styles.add_mar_bottom}`}
      >
        Create database with
      </h4>
      <div className="row">
        <div
          className={`col-md-6 ${styles.add_pad_left} ${styles.add_pad_right}`}
        >
          {session ? (
            <div
              className={styles.herokuButtonBox}
              onClick={() => {
                if (session) {
                  startCreation();
                }
              }}
            >
              <HerokuLogoComponent />
            </div>
          ) : (
            <LoginButton dispatch={dispatch} callback={startCreation}>
              <div className={styles.herokuButtonBox}>
                <HerokuLogoComponent />
              </div>
            </LoginButton>
          )}
        </div>
        <div
          className={`col-md-6 ${styles.add_pad_left} ${styles.add_pad_right}`}
        >
          <div
            className={`${styles.display_flex} ${styles.flex_space_between} ${styles.add_mar_bottom_mid}`}
          >
            <strong>Heroku</strong>
            {session ? (
              <Button
                size="xs"
                onClick={() => {
                  if (session) {
                    startCreation();
                  }
                }}
              >
                Create Database
              </Button>
            ) : (
              <LoginButton dispatch={dispatch} callback={startCreation}>
                <Button size="xs">Create Database</Button>
              </LoginButton>
            )}
          </div>
          <div>
            <p>
              Creates a new <strong>Hobby Dev</strong> Heroku Postgres database
            </p>
          </div>
        </div>
      </div>
    </>
  );
};

export default Intro;
