import React, { useEffect } from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { z } from 'zod';
import { Link, RouteComponentProps } from 'react-router';
import { FaEdit, FaTimes, FaSearch, FaFilter } from 'react-icons/fa';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';

import { Button } from '../../../../new-components/Button';
import { ReduxState } from '../../../../types';
import { mapDispatchToPropsEmpty } from '../../../Common/utils/reactUtils';
import AceEditor from '../../../Common/AceEditor/BaseEditor';
import URLPreview from './URLPreview';
import { allowedQueriesCollection } from '../../../../metadata/utils';
import { dropRESTEndpoint, exportMetadata } from '../../../../metadata/actions';
import _push from '../../Data/push';
import Landing from './Landing';
import { badgeSort } from './utils';
import CollapsibleToggle from '../../../Common/CollapsibleToggle/CollapsibleToggle';
import { ExportOpenApiButton } from './Form/ExportOpenAPI';
import { InputField, useConsoleForm } from '../../../../new-components/Form';
import { DropdownButton } from '../../../../new-components/DropdownButton';
import { BadgeColor } from '../../../../new-components/Badge';
import { CardedTable } from '../../../../new-components/CardedTable';

interface ListComponentProps extends Props {
  location: RouteComponentProps<unknown, unknown>['location'];
}

export const formSchema = z.object({
  search: z.string(),
});

const badgeColors: Record<string, BadgeColor> = {
  GET: 'green',
  POST: 'blue',
  PUT: 'yellow',
  DELETE: 'red',
  PATCH: 'purple',
};

const ListComponent: React.FC<ListComponentProps> = ({
  location,
  restEndpoints,
  queryCollections,
  dispatch,
}) => {
  // refetch metadata on mount
  useEffect(() => {
    dispatch(exportMetadata());
  }, []);

  const {
    methods: { watch },
    Form,
  } = useConsoleForm({
    schema: formSchema,
  });
  const search = watch('search');

  const [selectedMethods, setSelectedMethods] = React.useState<string[]>([]);

  const highlighted = (location.query?.highlight as string)?.split(',') || [];
  const allowedQueries = queryCollections?.find(
    collection => collection.name === allowedQueriesCollection
  );

  const filteredEndpoints = React.useMemo(() => {
    return restEndpoints?.filter(op => {
      const searchMatch =
        !search ||
        op.methods.some(i => {
          return i.toLowerCase().includes(search.toLowerCase());
        }) ||
        op.name.toLowerCase().includes(search.toLowerCase()) ||
        op.url.toLowerCase().includes(search.toLowerCase());

      const methodMatch =
        selectedMethods.length === 0 ||
        op.methods.some(method => selectedMethods.includes(method));
      return searchMatch && methodMatch;
    });
  }, [search, selectedMethods]);

  if (!queryCollections || !allowedQueries || !restEndpoints) {
    return <Landing />;
  }

  const allAllowedQueries = allowedQueries.definition.queries;

  const findQuery = (name: string) =>
    allAllowedQueries.find(q => q.name === name)?.query ?? '';

  const onClickDelete = (name: string, request: string) => () => {
    dispatch(dropRESTEndpoint(name, request));
  };
  const onClickEdit = (link: string) => () => {
    dispatch(_push(`/api/rest/edit/${link}`));
  };

  const sortedEndpoints =
    filteredEndpoints &&
    [...filteredEndpoints]?.sort(endpoint =>
      highlighted.includes(endpoint?.name) ? -1 : 1
    );

  return (
    <Form onSubmit={() => {}}>
      <Analytics name="RestList" {...REDACT_EVERYTHING}>
        <div className="pl-md pt-md pr-md">
          <div className="flex">
            <h2 className="text-xl font-bold pr-2">REST Endpoints</h2>
            <Analytics name="restified-create-btn-from-list-page">
              <Button
                mode="primary"
                size="sm"
                onClick={() => dispatch(_push('/api/rest/create'))}
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
            <div className="flex w-1/3">
              <InputField
                icon={<FaSearch />}
                placeholder="Search endpoints..."
                name="search"
                inputClassName="rounded-r-none"
                className="mb-0"
                noErrorPlaceholder
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
            columns={[
              'DETAILS',
              'ENDPOINT',
              'METHODS',
              <div className="pr-10 float-right">MODIFY</div>,
            ]}
            data={
              sortedEndpoints
                ? sortedEndpoints?.map(endpoint => [
                    <>
                      <Link
                        to={{
                          pathname: `/api/rest/details/${endpoint.name}`,
                          state: {
                            ...endpoint,
                            currentQuery: findQuery(endpoint.name),
                          },
                        }}
                      >
                        <h4>
                          {endpoint.name}{' '}
                          {highlighted.includes(endpoint.name) && (
                            <span className="relative bottom-2 text-green-700">
                              ●
                            </span>
                          )}
                        </h4>
                      </Link>
                      {endpoint.comment && <p>{endpoint.comment}</p>}
                    </>,
                    <div className="flex flex-col w-3/4">
                      <URLPreview urlInput={endpoint.url} />
                      <CollapsibleToggle
                        title="GraphQL Request"
                        useDefaultTitleStyle
                      >
                        <AceEditor
                          name="query-viewer"
                          value={findQuery(endpoint.name)}
                          placeholder="query SampleQuery {}"
                          height="300px"
                          mode="graphqlschema"
                          readOnly
                          setOptions={{ useWorker: false }}
                        />
                      </CollapsibleToggle>
                    </div>,
                    badgeSort(endpoint.methods).map(method => (
                      <span className="mr-sm" key={`badge-list-${method}`}>
                        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-sm font-semibold bg-blue-100 text-blue-800">
                          {method}
                        </span>
                      </span>
                    )),
                    <div className="px-sm py-xs align-top float-right">
                      <Analytics name="restified-delete-btn">
                        <Button
                          size="sm"
                          onClick={onClickDelete(
                            endpoint.name,
                            findQuery(endpoint.name)
                          )}
                          icon={<FaTimes />}
                          className="mr-1"
                        >
                          Delete
                        </Button>
                      </Analytics>
                      <Analytics name="restified-edit-btn">
                        <Button
                          size="sm"
                          icon={<FaEdit />}
                          onClick={onClickEdit(endpoint.name)}
                        >
                          Edit
                        </Button>
                      </Analytics>
                    </div>,
                  ])
                : []
            }
          />
        </div>
      </Analytics>
    </Form>
  );
};

const mapStateToProps = (state: ReduxState) => ({
  restEndpoints: state.metadata.metadataObject?.rest_endpoints,
  queryCollections: state.metadata.metadataObject?.query_collections,
});
const listComponentConnector = connect(
  mapStateToProps,
  mapDispatchToPropsEmpty
);
type InjectedProps = ConnectedProps<typeof listComponentConnector>;
type Props = InjectedProps;
const ConnectedComponent = listComponentConnector(ListComponent);

export default ConnectedComponent;
