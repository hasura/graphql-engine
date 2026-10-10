import { Button } from '@hasura/shared/ui';
import { useState } from 'react';
import { AddAgentForm } from './AddAgentForm';
import { ManageAgentsTable } from './ManageAgentsTable';

export const ManageAgents = () => {
  const [showCreateAgentForm, setShowCreateAgentForm] = useState(false);

  return (
    <div>
      <ManageAgentsTable />
      <Button mode="default" onClick={() => setShowCreateAgentForm(true)}>
        Add Agent
      </Button>
      {showCreateAgentForm ? (
        <AddAgentForm
          onClose={() => setShowCreateAgentForm(false)}
          onSuccess={() => setShowCreateAgentForm(false)} // close the form on successful save
        />
      ) : null}
    </div>
  );
};
