import { useLocation, useNavigate } from 'react-router';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import {
  Breadcrumbs,
  Button,
  LegacyBadge,
  Input,
  GraphqlCodeBlock,
  Text,
  Separator,
} from '@hasura/shared/ui';
import { badgeSort, getCurrentPageHost } from './utils';
import { setLSItem } from '@hasura/shared/utils';
import LivePreview from './LivePreview';
import { LS_KEYS } from '@hasura/shared/types';
import { Flex, Heading } from '@radix-ui/themes';

const DetailsComponent = ({ dataHeaders }) => {
  const navigate = useNavigate();
  const location = useLocation();
  const endpointState = location.state;
  const endpointLocation = `${getCurrentPageHost()}/api/rest/${
    endpointState.url
  }`;

  const crumbs = [
    {
      title: 'REST Endpoints',
      url: `/api/rest`,
    },
    {
      title: endpointState?.name,
      url: '',
    },
  ];

  const onClickEdit = () => {
    navigate(`/api/rest/edit/${endpointState.name}`);
  };

  const onClickOpenInGQL = (query: string) => () => {
    setLSItem(LS_KEYS.graphiqlQuery, query);
    navigate('/api/api-explorer');
  };

  return (
    <Analytics name="RestDetails" {...REDACT_EVERYTHING}>
      <Flex className="px-4 pt-4" direction="column" gap="4">
        <div className="">
          <Breadcrumbs items={crumbs} />
        </div>
        <Flex align="center" gap="2">
          <Heading size="5">{endpointState.name}</Heading>
          <Button mode="primary" onClick={onClickEdit}>
            Edit Endpoint
          </Button>
        </Flex>
        <Text as="p">{endpointState.comment}</Text>
        <Separator className="my-4" />
        <Flex justify="between" className="w-full">
          <div className="w-7/12">
            <div className="mb-4">
              <div className="mb-4">
                <Text weight="bold">REST Endpoint</Text>
              </div>
              <Input
                type="text"
                placeholder="Rest endpoint URL"
                value={endpointLocation}
                data-key="name"
                disabled
                required
              />
            </div>
            <Text weight="bold">Methods Available</Text>
            <Flex className="mt-4" align="center" gap="4">
              {badgeSort(endpointState.methods).map((method) => (
                <span key={`badge-details-${method}`}>
                  <LegacyBadge type={`rest-${method}`} />
                </span>
              ))}
            </Flex>
            <Separator size="4" className="mb-6 mt-4" />
            <Flex
              justify="between"
              align="center"
              gap="2"
              className="align-center mb-4"
            >
              <Text weight="bold">GraphQL Request</Text>
              <Button
                size="1"
                // color="white"
                onClick={onClickOpenInGQL(endpointState.currentQuery)}
              >
                Open in GraphiQL
              </Button>
            </Flex>
            <GraphqlCodeBlock text={endpointState.currentQuery} scrollable />
          </div>
          <div className="h-full w-1/2 pl-xl">
            <LivePreview
              endpointState={endpointState}
              pageHost={endpointLocation}
              dataHeaders={dataHeaders}
            />
          </div>
        </Flex>
      </Flex>
    </Analytics>
  );
};

export default DetailsComponent;
