import React from 'react';
import { Flex, Heading } from '@radix-ui/themes';
import { useLocation, useNavigate } from 'react-router';
import { FaEdit, FaTimes, FaSearch, FaFilter } from 'react-icons/fa';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import {
  LearnMoreLink,
  Button,
  DropdownButton,
  BadgeColor,
  CardedTable,
  hasuraToast,
  Collapsible,
  Text,
  DropdownMenu,
  Input,
  SkeletonList,
  IndicatorCard,
  Badge,
  GraphqlCodeBlock,
  RelativeLink,
} from '@hasura/shared/ui';
import { badgeSort } from './utils';
import URLPreview from './URLPreview';
import debounce from 'lodash/debounce';
import { useDeleteRestEndpoints, useMetadata } from '@hasura/metadata/api';
import { getConfirmation } from '@hasura/shared/utils';
import Landing from './Landing';
import { ExportOpenApiButton } from '../Form/ExportOpenAPI';

const badgeColors: Record<string, BadgeColor> = {
  GET: 'green',
  POST: 'blue',
  PUT: 'yellow',
  DELETE: 'red',
  PATCH: 'purple',
};

export const RestEndpointList: React.FC = () => {
  const location = useLocation();
  const navigate = useNavigate();
  const {
    data: { restEndpoints = [], queryCollections = [] } = {},
    isError,
    isLoading,
  } = useMetadata((m) => ({
    restEndpoints: m.metadata?.rest_endpoints,
    queryCollections: m.metadata?.query_collections,
  }));

  const { deleteRestEndpoints } = useDeleteRestEndpoints();
  const [selectedMethods, setSelectedMethods] = React.useState<string[]>([]);

  const highlighted =
    new URLSearchParams(location.search).get('highlight')?.split(',') || [];

  const [search, setSearch] = React.useState('');

  const { processedEndpoints, emptySearch } = React.useMemo(() => {
    const matches =
      restEndpoints?.map((endpoint) => {
        const searchMatch =
          !search ||
          endpoint.methods.some((i) => {
            return i.toLowerCase().includes(search.toLowerCase());
          }) ||
          endpoint.name.toLowerCase().includes(search.toLowerCase()) ||
          endpoint.url.toLowerCase().includes(search.toLowerCase());

        const methodMatch =
          selectedMethods.length === 0 ||
          endpoint.methods.some((method) => selectedMethods.includes(method));

        return {
          endpoint,
          isVisible: searchMatch && methodMatch,
        };
      }) ?? [];

    const localRestEndpoints = matches
      .map(({ endpoint, isVisible }) => ({
        endpoint,
        className: isVisible ? '' : 'hidden',
      }))
      .sort((endpoint) =>
        highlighted.includes(endpoint?.endpoint?.name) ? -1 : 1,
      );

    const localEmptySearch = matches.every((m) => !m.isVisible);

    return {
      processedEndpoints: localRestEndpoints,
      emptySearch: localEmptySearch,
    };
  }, [highlighted, restEndpoints, search, selectedMethods]);

  if (isLoading) {
    return <SkeletonList count={5} />;
  }

  if (isError) {
    return (
      <IndicatorCard status="negative" showIcon>
        Error getting REST Endpoints
      </IndicatorCard>
    );
  }

  if (!queryCollections || !restEndpoints) {
    return <Landing />;
  }

  const findQuery = (name: string, collectionName: string) => {
    const collection = queryCollections.find((q) => q.name === collectionName);
    if (collection) {
      const query = collection.definition.queries.find((q) => q.name === name);
      return query ? query.query : '';
    }
    return '';
  };

  const onClickDelete = (name: string, request: string) => () => {
    const confirmMessage = `This will delete the REST endpoint "${name}". Are you sure?`;
    const isOk = getConfirmation(confirmMessage, true, name);
    if (!isOk) {
      return;
    }

    deleteRestEndpoints([name], {
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          message: `Successfully deleted ${name} REST endpoint`,
        });
      },
      onError: (error) => {
        hasuraToast({
          type: 'error',
          message: `Error deleting ${name} REST endpoint: ${error}`,
        });
      },
    });
  };

  const onClickEdit = (link: string) => () => {
    navigate(`/api/rest/edit/${encodeURIComponent(link)}`);
  };

  const onSearchChange = debounce((e: React.ChangeEvent<HTMLInputElement>) => {
    setSearch(e.target.value);
  });

  return (
    <Analytics name="RestList" {...REDACT_EVERYTHING}>
      <div className="p-4">
        <Flex gap="2" align="center">
          <Heading size="4">REST Endpoints</Heading>
          <Analytics
            name="restified-create-btn-from-list-page"
            passHtmlAttributesToChildren
          >
            <Button
              mode="primary"
              size="sm"
              onClick={() => navigate('/api/rest/create')}
            >
              Create REST
            </Button>
          </Analytics>
        </Flex>
        <Text>
          Create Rest endpoints on the top of existing GraphQL queries and
          mutations{' '}
          <div className="w-8/12 mt-2">
            <Text>
              REST endpoints allow for the creation of a REST interface to your
              saved GraphQL queries and mutations. Endpoints are generated from
              /api/rest/* and inherit the authorization and permission structure
              from your associated GraphQL nodes.{' '}
            </Text>
            <LearnMoreLink href="https://hasura.io/docs/latest/graphql/core/api-reference/restified.html" />
          </div>
        </Text>
        <Flex className="my-4">
          <Flex className="relative w-8/12" align="center">
            <Input
              icon={FaSearch}
              placeholder="Search endpoints..."
              name="search"
              onChange={onSearchChange}
              full
            />
            <DropdownButton
              size="2"
              mode="default"
              data-testid="dropdown-button"
              leftIcon={FaFilter}
              items={Object.keys(badgeColors).map((method) => (
                <DropdownMenu.CheckboxItem
                  key={method}
                  checked={selectedMethods.includes(method)}
                  onCheckedChange={(checked) => {
                    if (!checked) {
                      setSelectedMethods(
                        selectedMethods.filter((m: string) => m !== method),
                      );
                    } else {
                      setSelectedMethods([...selectedMethods, method]);
                    }
                  }}
                >
                  {method.toUpperCase()}
                </DropdownMenu.CheckboxItem>
              ))}
            >
              Method{' '}
              {selectedMethods.length > 0 && `(${selectedMethods.length})`}
            </DropdownButton>
          </Flex>
          <div className="ml-auto">
            <ExportOpenApiButton />
          </div>
        </Flex>
        <CardedTable
          columns={[
            'DETAILS',
            'ENDPOINT',
            'METHODS',
            <div key="modify" className="text-right">
              MODIFY
            </div>,
          ]}
          data={
            processedEndpoints?.map((endpoint) => [
              <React.Fragment key={`details-${endpoint.endpoint.name}`}>
                <RelativeLink
                  to={`/api/rest/details/${encodeURIComponent(
                    endpoint.endpoint.name,
                  )}`}
                  state={{
                    ...endpoint,
                    currentQuery: findQuery(
                      endpoint.endpoint.name,
                      endpoint.endpoint.definition.query.collection_name,
                    ),
                  }}
                >
                  <Text>
                    {endpoint.endpoint.name}{' '}
                    {highlighted.includes(endpoint.endpoint.name) && (
                      <Text color="green">●</Text>
                    )}
                  </Text>
                </RelativeLink>
                {endpoint.endpoint.comment && (
                  <Text>{endpoint.endpoint.comment}</Text>
                )}
              </React.Fragment>,
              <Flex
                direction="column"
                key={`preview-${endpoint.endpoint.name}`}
                className="w-3/4"
                gap="2"
              >
                <URLPreview urlInput={endpoint.endpoint.url} />
                <Collapsible
                  triggerChildren={<Text weight="bold">GraphQL Request</Text>}
                  disableContentStyles
                >
                  <GraphqlCodeBlock
                    className="mt-2"
                    text={findQuery(
                      endpoint.endpoint.name,
                      endpoint.endpoint.definition.query.collection_name,
                    )}
                  />
                </Collapsible>
              </Flex>,
              <Flex
                align="center"
                gap="2"
                key={`methods-${endpoint.endpoint.name}`}
              >
                {badgeSort(endpoint.endpoint.methods).map((method) => (
                  <Badge key={method} color={badgeColors[method]}>
                    {method}
                  </Badge>
                ))}
              </Flex>,
              <Flex
                key={`actions-${endpoint.endpoint.name}`}
                justify="end"
                align="center"
                gap="2"
              >
                <Analytics
                  name="restified-delete-btn"
                  passHtmlAttributesToChildren
                >
                  <Button
                    mode="destructive"
                    size="sm"
                    onClick={onClickDelete(
                      endpoint.endpoint.name,
                      findQuery(
                        endpoint.endpoint.name,
                        endpoint.endpoint.definition.query.collection_name,
                      ),
                    )}
                    leftIcon={FaTimes}
                  >
                    Delete
                  </Button>
                </Analytics>
                <Analytics
                  name="restified-edit-btn"
                  passHtmlAttributesToChildren
                >
                  <Button
                    mode="default"
                    size="sm"
                    leftIcon={FaEdit}
                    onClick={onClickEdit(endpoint.endpoint.name)}
                  >
                    Edit
                  </Button>
                </Analytics>
              </Flex>,
            ]) ?? [[]]
          }
        />
        {emptySearch && (
          <div className="p-4">
            <Text align="center" as="div">
              No REST Endpoints available for current search
            </Text>
          </div>
        )}
      </div>
    </Analytics>
  );
};
