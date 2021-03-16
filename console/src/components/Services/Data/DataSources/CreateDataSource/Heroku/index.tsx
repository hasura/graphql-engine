import * as React from 'react';
import styles from '../styles.scss';
import { HerokuSession } from './types';
import Intro from './Intro';
import DBCreation from './DBCreation';
import { Dispatch } from '../../../../../../types';

type Props = {
  session?: HerokuSession;
  dispatch: Dispatch;
};

const Heroku: React.FC<Props> = ({ session, dispatch }) => {
  const [shouldStart, setShouldStart] = React.useState(false);

  return (
    <div className={styles.wd100}>
      {session && shouldStart && (
        <DBCreation
          session={session}
          shouldStart={shouldStart}
          dispatch={dispatch}
        />
      )}
      {!shouldStart && (
        <Intro
          session={session}
          startCreation={() => setShouldStart(true)}
          dispatch={dispatch}
        />
      )}
    </div>
  );
};

export default Heroku;
