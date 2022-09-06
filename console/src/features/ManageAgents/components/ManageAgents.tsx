import { Button } from '@/new-components/Button';
import React, { useState } from 'react';
import { AddAgentForm } from './AddAgentForm';
import { ManageAgentsTable } from './ManageAgentsTable';

export const ManageAgents = () => {
  const [showCreateAgentForm, setShowCreateAgentForm] = useState(false);

  return (
    <div>
      <p className="text-xl text-gray-600 py-3 font-bold">
        Data Connector Agents
      </p>
      <hr className="m-0" />
      <ManageAgentsTable />
      <Button onClick={() => setShowCreateAgentForm(true)}>Add Agent</Button>
      {showCreateAgentForm ? (
        <AddAgentForm
          onClose={() => setShowCreateAgentForm(false)}
          onSuccess={() => setShowCreateAgentForm(false)} // close the form on successful save
        />
      ) : null}
    </div>
  );
};
