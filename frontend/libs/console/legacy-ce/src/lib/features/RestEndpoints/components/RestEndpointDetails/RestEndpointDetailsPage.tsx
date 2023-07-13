import React from 'react';
import { RestEndpointDetails } from './RestEndpointDetails';
import { Button } from '../../../../new-components/Button';
import {
  BreadcrumbItem,
  Breadcrumbs,
} from '../../../../new-components/Breadcrumbs/Breadcrumbs';
import { RouteComponentProps, browserHistory } from 'react-router';

interface RestEndpointDetailsPageProps {
  params: RouteComponentProps<{ name: string }, unknown>['params'];
}

export const RestEndpointDetailsPage = (
  props: RestEndpointDetailsPageProps
) => {
  const name = props.params.name;

  const breadcrumbs: BreadcrumbItem[] = [
    {
      title: 'Rest Endpoints',
      onClick: () => browserHistory.push('/api/rest'),
    },
    {
      title: name,
    },
  ];

  if (!name) {
    return null;
  }

  return (
    <div className="p-9">
      <div className="-ml-2 mb-2">
        <Breadcrumbs items={breadcrumbs} />
      </div>
      <div className="flex items-center">
        <div className="pb-2">
          <h1 className="text-xl font-semibold mb-0">{name}</h1>
          <p className="text-gray-500 mt-0">Test your REST endpoint</p>
        </div>
        <Button
          className="ml-auto"
          onClick={() => browserHistory.push(`/api/rest/edit/${name}`)}
        >
          Edit Endpoint
        </Button>
      </div>
      <hr className="mb-4 mt-2 -mx-9" />
      <RestEndpointDetails name={name} />
    </div>
  );
};
