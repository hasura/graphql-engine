import React from 'react';
import { RestEndpointModal } from '../../Services/ApiExplorer/Rest/RestEndpointModal/RestEndpointModal';
import { Button } from '../../../new-components/Button';
import { FaLink } from 'react-icons/fa';
import { Badge } from '../../../new-components/Badge';
import { Analytics } from '../../../features/Analytics';
import { Table } from '../../../features/hasura-metadata-types';

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
        <Button size="sm" onClick={toggleModal} icon={<FaLink />}>
          Create REST Endpoints{' '}
          <Badge className="pt-0 pb-0 pl-2 pr-2 ml-1" color="indigo">
            New
          </Badge>
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
