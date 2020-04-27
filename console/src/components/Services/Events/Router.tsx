import React from 'react';
import { Route, IndexRedirect } from 'react-router';
import rightContainerConnector from '../../Common/Layout/RightContainer/RightContainer';
import Container from './Containers/Main';
import { fetchTriggers } from './ServerIO';
import globals from '../../../Globals';
import EventTriggersLanding from '../EventTrigger/Landing/EventTrigger';
import {
  ScheduledTriggers,
  AddScheduledTrigger,
  ScheduledTriggerLogs,
  ScheduledTriggeModify,
  ScheduledTriggerLanding,
} from './ScheduledTriggers';

const triggersInit = ({ dispatch }: { dispatch: any }) => {
  return (nextState: any, replaceState: any, cb: any) => {
    Promise.all([dispatch(fetchTriggers())]).then(
      () => {
        cb();
      },
      () => {
        replaceState(globals.urlPrefix);
        cb();
      }
    );
  };
};

const getTriggersRouter = (
  connect: any,
  store: any,
  composeOnEnterHooks: any
) => {
  return (
    <Route
      path={'triggers'}
      component={Container(connect)}
      onEnter={composeOnEnterHooks([triggersInit(store)])}
      onChange={triggersInit(store)}
    >
      <IndexRedirect to="events" />
      <Route path="events" component={rightContainerConnector(connect)}>
        <IndexRedirect to="manage" />
        <Route path="add" component={ScheduledTriggers} />
        <Route path="manage" component={EventTriggersLanding(connect)} />
      </Route>
      <Route path="scheduled" component={rightContainerConnector(connect)}>
        <IndexRedirect to="manage" />
        <Route path="add" component={AddScheduledTrigger(connect)} />
        <Route path="manage" component={ScheduledTriggerLanding(connect)} />
        <Route
          path=":triggerName/logs"
          component={ScheduledTriggerLogs(connect)}
        />
        <Route
          path=":triggerName/modify"
          component={ScheduledTriggeModify(connect)}
        />
      </Route>
    </Route>
  );
};

export default getTriggersRouter;
