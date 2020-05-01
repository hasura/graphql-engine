import React from 'react';
import { Route, IndexRedirect } from 'react-router';
import rightContainerConnector from '../../Common/Layout/RightContainer/RightContainer';
import Container from './Containers/Main';
import { fetchTriggers } from './ServerIO';
import globals from '../../../Globals';
import {
  AddScheduledTrigger,
  ScheduledTriggerLogs,
  ScheduledTriggeModify,
  ScheduledTriggerLanding,
} from './ScheduledTriggers';
import {
  AddEventTrigger,
  ModifyEventTrigger,
  EventTriggerLanding,
} from './EventTriggers';

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
      path="events"
      component={Container(connect)}
      onEnter={composeOnEnterHooks([triggersInit(store)])}
      onChange={triggersInit(store)}
    >
      <IndexRedirect to="data" />
      <Route path="data" component={rightContainerConnector(connect)}>
        <IndexRedirect to="manage" />
        <Route path="add" component={AddEventTrigger(connect)} />
        <Route
          path=":triggerName/modify"
          component={ModifyEventTrigger(connect)}
        />
        <Route path="manage" component={EventTriggerLanding(connect)} />
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
