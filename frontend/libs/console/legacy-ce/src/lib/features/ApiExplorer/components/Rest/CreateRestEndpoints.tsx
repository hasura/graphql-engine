import React from 'react';
import { Button } from '@hasura/shared/ui';
import { FaLink } from 'react-icons/fa';
import { Analytics } from '@hasura/shared/analytics';
import { Table } from '@hasura/shared/types';
import { RestEndpointModal } from './RestEndpointModal/RestEndpointModal';

interface CreateRestEndpointProps {
  tableName: string;
  dataSourceName: string;
  table: Table;
}

export const CreateRestEndpoint = (props: CreateRestEndpointProps) => {
  const { tableName, dataSourceName, table } = props;
  const [isModalOpen, setIsModalOpen] = React.useState(false);

  const toggleModal = () => {
    setIsModalOpen(!isModalOpen);
  };

  return (
    <>
      <Analytics
        name="data-tab-btn-create-rest-endpoints"
        passHtmlAttributesToChildren
      >
        <Button
          mode="default"
          size="sm"
          onClick={toggleModal}
          leftIcon={FaLink}
        >
          Create REST Endpoints
        </Button>
      </Analytics>
      {isModalOpen && (
        <RestEndpointModal
          onClose={toggleModal}
          tableName={tableName}
          dataSourceName={dataSourceName}
          table={table}
        />
      )}
    </>
  );
};
