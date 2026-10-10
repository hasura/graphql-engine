import React from 'react';
import { dataRoutes } from '@hasura/shared/utils';
import { Form } from '../Form';
import AdhocEventsContainer from '../Container';
import { useNavigate } from 'react-router';
import { useAppContext } from '@hasura/shared/context';

const AddAdhocEvent: React.FC = () => {
  const navigate = useNavigate();
  const { readOnlyMode } = useAppContext();

  const onSuccess = () => {
    navigate(dataRoutes.getAdhocPendingEventsRoute('absolute'));
  };

  return (
    <div className="bootstrap-jail">
      <AdhocEventsContainer tabName="add">
        {readOnlyMode ? (
          'Cannot schedule event in read only mode'
        ) : (
          <div className="w-1/2">
            <Form onSuccess={onSuccess} />
          </div>
        )}
      </AdhocEventsContainer>
    </div>
  );
};

export default AddAdhocEvent;
