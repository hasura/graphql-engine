import React from 'react';
import { ScheduledTrigger } from '../../Types';

type LogsProps = {
  dispatch: any;
  currentTrigger?: ScheduledTrigger;
};

const Logs = (props: LogsProps) => {
  console.log(props);
  return <div>Hello lolz</div>;
};

export default Logs;
