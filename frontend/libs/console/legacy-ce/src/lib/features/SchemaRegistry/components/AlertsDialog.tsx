import React from 'react';
import { Tabs, Dialog } from '@hasura/shared/ui';
import { EmailAlerts } from './EmailAlerts';
import { SlackAlerts } from './SlackAlerts';

type DialogProps = {
  onClose: () => void;
};

export const AlertsDialog: React.FC<DialogProps> = ({ onClose }) => {
  const [tabState, setTabState] = React.useState('email');

  return (
    <Dialog onClose={tabState === 'slack' ? onClose : undefined}>
      <div className="h-full ml-4">
        <Tabs
          value={tabState}
          onValueChange={(state) => setTabState(state)}
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
