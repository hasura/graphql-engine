import React from 'react';
import globals from '../../../../Globals';
import { useEELiteAccess } from '../../hooks/useEELiteAccess';
import { EETrialCard } from '../EETrialCard/EETrialCard';

type Props = {
  children?: React.ReactElement;
};

export function ETAutoCleanupWrapper(props: Props) {
  const { children } = props;
  const { access } = useEELiteAccess(globals);

  if (
    globals.consoleType === 'cloud' ||
    globals.consoleType === 'pro' ||
    access === 'active'
  ) {
    return children ?? null;
  }

  if (access === 'forbidden') {
    return null;
  }

  return (
    <div className="max-w-3xl">
      <EETrialCard
        cardTitle="Improve database performance with event log cleanup"
        id="event-trigger-auto-cleanup"
        cardText={
          <span>
            Reduce database log bloat by setting granular event-log cleanup
            rules on a global and per-event basis.
          </span>
        }
        buttonLabel="Enable Enterprise"
        buttonType="default"
        eeAccess={access}
        horizontal
      />
    </div>
  );
}
