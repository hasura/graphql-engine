import { useNavigate, useParams } from 'react-router';
import { RestEndpointDetails } from './RestEndpointDetails';
import { Button, BreadcrumbItem, Breadcrumbs } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

export const RestEndpointDetailsPage = () => {
  const { name } = useParams<{ name: string }>();
  const navigate = useNavigate();

  if (!name) {
    return null;
  }

  const breadcrumbs: BreadcrumbItem[] = [
    {
      title: 'REST Endpoints',
      onClick: () => navigate('/api/rest'),
    },
    {
      title: name,
    },
  ];

  return (
    <div className="p-9">
      <div className="-ml-2 mb-2">
        <Breadcrumbs items={breadcrumbs} />
      </div>
      <Flex align="center">
        <div className="pb-2">
          <h1 className="text-xl font-semibold mb-0">{name}</h1>
          <p className="text-gray-500 mt-0">Test your REST endpoint</p>
        </div>
        <Button
          className="ml-auto"
          onClick={() => navigate(`/api/rest/edit/${name}`)}
        >
          Edit Endpoint
        </Button>
      </Flex>
      <hr className="mb-4 mt-2 -mx-9" />
      <RestEndpointDetails name={name} />
    </div>
  );
};
