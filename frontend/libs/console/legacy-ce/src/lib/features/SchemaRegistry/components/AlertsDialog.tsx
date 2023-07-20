import React from 'react';
import { Tabs } from '../../../new-components/Tabs';
import { EmailAlerts } from './EmailAlerts';
import { SlackAlerts } from './SlackAlerts';
import { Dialog } from '../../../new-components/Dialog';

type DialogProps = {
  onClose: () => void;
};

export const AlertsDialog: React.FC<DialogProps> = ({ onClose }) => {
  const [tabState, setTabState] = React.useState('email');

  return (
    <Dialog hasBackdrop onClose={tabState === 'slack' ? onClose : undefined}>
      <div className="h-full ml-4">
        <Tabs
          value={tabState}
          onValueChange={state => setTabState(state)}
          headerTabBackgroundColor="white"
          items={[
            {
              value: 'email',
              label: 'Email',
              content: <EmailAlerts onClose={onClose} />,
            },
            {
              value: 'slack',
              label: 'Slack',
              content: <SlackAlerts onClose={onClose} />,
            },
          ]}
        />
      </div>
    </Dialog>
  );
};
