import React from 'react';
import { browserHistory, Link, RouteComponentProps } from 'react-router';
import { FaEdit, FaTimes, FaSearch, FaFilter } from 'react-icons/fa';
import { Analytics, REDACT_EVERYTHING } from '../../../Analytics';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';

import { Button } from '../../../../new-components/Button';
import AceEditor from '../../../../components/Common/AceEditor/BaseEditor';
import { allowedQueriesCollection } from '../../../../metadata/utils';
import { DropdownButton } from '../../../../new-components/DropdownButton';
import { BadgeColor } from '../../../../new-components/Badge';
import { CardedTable } from '../../../../new-components/CardedTable';
import Landing from '../../../../components/Services/ApiExplorer/Rest/Landing';
import { badgeSort } from './utils';
import { CollapsibleToggle } from '../../../../components/Common';
import URLPreview from '../../../../components/Services/ApiExplorer/Rest/URLPreview';
import { ExportOpenApiButton } from '../../../../components/Services/ApiExplorer/Rest/Form/ExportOpenAPI';
import debounce from 'lodash/debounce';
import clsx from 'clsx';
import { useMetadata } from '../../../hasura-metadata-api';
import { hasuraToast } from '../../../../new-components/Toasts';
import { getConfirmation } from '../../../../components/Common/utils/jsUtils';
import { useDeleteRestEndpoints } from '../../hooks/useDeleteRestEndpoints';

interface ListComponentProps {
  location: RouteComponentProps<unknown, unknown>['location'];
}

const badgeColors: Record<string, BadgeColor> = {
  GET: 'green',
  POST: 'blue',
  PUT: 'yellow',
  DELETE: 'red',
  PATCH: 'purple',
};

const focusYellowRing =
  'block w-full h-input shadow-sm rounded border border-gray-300 hover:border-gray-400 focus-visible:outline-0 focus-visible:ring-2 focus-visible:ring-yellow-200 focus-visible:border-yellow-400 placeholder:text-slate-400 pl-10';

export const RestEndpointList: React.FC<ListComponentProps> = ({
  location,
}) => {
  const {
    data: { restEndpoints = [], queryCollections = [] } = {},
    isError,
    isLoading,
  } = useMetadata(m => ({
    restEndpoints: m.metadata?.rest_endpoints,
    queryCollections: m.metadata?.query_collections,
  }));

  const { deleteRestEndpoints } = useDeleteRestEndpoints();

  const [selectedMethods, setSelectedMethods] = React.useState<string[]>([]);

  const highlighted = (location.query?.highlight as string)?.split(',') || [];

  const allowedQueries = queryCollections?.find(
    collection => collection.name === allowedQueriesCollection
  );

  const [search, setSearch] = React.useState('');

  const emptySearch = React.useRef(false);

  const processedEndpoints = React.useMemo(() => {
    let localEmptySearch = true;
    const localRestEnpoints = restEndpoints
      ?.map(endpoint => {
        const searchMatch =
          !search ||
          endpoint.methods.some(i => {
            return i.toLowerCase().includes(search.toLowerCase());
          }) ||
          endpoint.name.toLowerCase().includes(search.toLowerCase()) ||
          endpoint.url.toLowerCase().includes(search.toLowerCase());

        const methodMatch =
          selectedMethods.length === 0 ||
          endpoint.methods.some(method => selectedMethods.includes(method));

        localEmptySearch = localEmptySearch && !(searchMatch && methodMatch);
        return {
          endpoint,
          className: searchMatch && methodMatch ? '' : 'hidden',
        };
      })
      ?.sort(endpoint =>
        highlighted.includes(endpoint?.endpoint?.name) ? -1 : 1
      );
    emptySearch.current = localEmptySearch;
    return localRestEnpoints;
  }, [highlighted, restEndpoints, search, selectedMethods]);

  if (isLoading) {
    return <div className="pl-10 mt-5 mb-xs">Loading Rest Enpoints...</div>;
  }

  if (isError) {
    return <div>Error getting Rest Endpoints</div>;
  }

  if (!queryCollections || !allowedQueries || !restEndpoints) {
    return <Landing />;
  }

  const allAllowedQueries = allowedQueries.definition.queries;

  const findQuery = (name: string) =>
    allAllowedQueries.find(q => q.name === name)?.query ?? '';

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
      onError: error => {
        hasuraToast({
          type: 'error',
          message: `Error deleting ${name} REST endpoint: ${error}`,
        });
      },
    });
  };

  const onClickEdit = (link: string) => () => {
    browserHistory.push(`/api/rest/edit/${link}`);
  };

  const onSearchChange = debounce((e: React.ChangeEvent<HTMLInputElement>) => {
    setSearch(e.target.value);
  });

  return (
    <Analytics name="RestList" {...REDACT_EVERYTHING}>
      <div className="pl-md pt-md pr-md">
        <div className="flex">
          <h2 className="text-xl font-bold pr-2">REST Endpoints</h2>
          <Analytics
            name="restified-create-btn-from-list-page"
            passHtmlAttributesToChildren
          >
            <Button
              mode="primary"
              size="sm"
              onClick={() => browserHistory.push('/api/rest/create')}
            >
              Create REST
            </Button>
          </Analytics>
        </div>
        <div className="">
          Create Rest endpoints on the top of existing GraphQL queries and
          mutations{' '}
          <div className="w-8/12 mt-sm">
            REST endpoints allow for the creation of a REST interface to your
            saved GraphQL queries and mutations. Endpoints are generated from
            /api/rest/* and inherit the authorization and permission structure
            from your associated GraphQL nodes.
            <LearnMoreLink href="https://hasura.io/docs/latest/graphql/core/api-reference/restified.html" />
          </div>
        </div>
        <div className="flex pt-2 pb-1">
          <div className={clsx('flex relative w-1/3')}>
            <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
              {React.cloneElement(<FaSearch />, {
                className: 'h-5 w-5 text-gray-400',
                role: 'img',
              })}
            </div>
            <input
              placeholder="Search endpoints..."
              name="search"
              className={focusYellowRing}
              onChange={onSearchChange}
            />
            <DropdownButton
              options={{
                item: {
                  onSelect(e) {
                    e.preventDefault();
                  },
                },
              }}
              className="w-32 rounded-l-none"
              size="md"
              data-testid="dropdown-button"
              items={[
                Object.keys(badgeColors).map(method => (
                  <div
                    className="py-1 w-full"
                    onClick={() => {
                      if (selectedMethods.includes(method)) {
                        setSelectedMethods(
                          selectedMethods.filter((m: string) => m !== method)
                        );
                      } else {
                        setSelectedMethods([...selectedMethods, method]);
                      }
                    }}
                  >
                    <div className="flex items-center">
                      <div className="mr-2 relative -top-1">
                        <input
                          type="checkbox"
                          className="border border-gray-300 rounded "
                          checked={selectedMethods.includes(method)}
                        />
                      </div>
                      <div>{method.toUpperCase()}</div>
                    </div>
                  </div>
                )),
              ]}
            >
              <FaFilter className="mr-1 w-3 h-3" /> Method{' '}
              {selectedMethods.length > 0 && `(${selectedMethods.length})`}
            </DropdownButton>
          </div>
          <div className="ml-auto">
            <ExportOpenApiButton />
          </div>
        </div>
        <CardedTable
          showActionCell={false}
          keyBuilder={index =>
            processedEndpoints?.[index].endpoint.name || index.toString()
          }
          rowClassNames={processedEndpoints?.map(
            endpoint => endpoint.className
          )}
          columns={[
            'DETAILS',
            'ENDPOINT',
            'METHODS',
            <div className="pr-10 float-right">MODIFY</div>,
          ]}
          data={
            processedEndpoints?.map(endpoint => [
              <>
                <Link
                  to={{
                    pathname: `/api/rest/details/${endpoint.endpoint.name}`,
                    state: {
                      ...endpoint,
                      currentQuery: findQuery(endpoint.endpoint.name),
                    },
                  }}
                >
                  <h4>
                    {endpoint.endpoint.name}{' '}
                    {highlighted.includes(endpoint.endpoint.name) && (
                      <span className="relative bottom-2 text-green-700">
                        ●
                      </span>
                    )}
                  </h4>
                </Link>
                {endpoint.endpoint.comment && (
                  <p>{endpoint.endpoint.comment}</p>
                )}
              </>,
              <div className="flex flex-col w-3/4">
                <URLPreview urlInput={endpoint.endpoint.url} />
                <CollapsibleToggle title="GraphQL Request" useDefaultTitleStyle>
                  <AceEditor
                    name="query-viewer"
                    value={findQuery(endpoint.endpoint.name)}
                    placeholder="query SampleQuery {}"
                    height="300px"
                    mode="graphqlschema"
                    readOnly
                    setOptions={{ useWorker: false }}
                  />
                </CollapsibleToggle>
              </div>,
              badgeSort(endpoint.endpoint.methods).map(method => (
                <span className="mr-sm" key={`badge-list-${method}`}>
                  <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-sm font-semibold bg-blue-100 text-blue-800">
                    {method}
                  </span>
                </span>
              )),
              <div className="px-sm py-xs align-top float-right">
                <Analytics
                  name="restified-delete-btn"
                  passHtmlAttributesToChildren
                >
                  <Button
                    size="sm"
                    onClick={onClickDelete(
                      endpoint.endpoint.name,
                      findQuery(endpoint.endpoint.name)
                    )}
                    icon={<FaTimes />}
                    className="mr-1"
                  >
                    Delete
                  </Button>
                </Analytics>
                <Analytics
                  name="restified-edit-btn"
                  passHtmlAttributesToChildren
                >
                  <Button
                    size="sm"
                    icon={<FaEdit />}
                    onClick={onClickEdit(endpoint.endpoint.name)}
                  >
                    Edit
                  </Button>
                </Analytics>
              </div>,
            ]) ?? [[]]
          }
        />
        {emptySearch.current && 'No Rest Enpoints available for currect search'}
      </div>
    </Analytics>
  );
};
