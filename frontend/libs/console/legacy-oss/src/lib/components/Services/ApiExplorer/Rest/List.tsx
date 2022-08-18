import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { Link } from 'react-router';
import { FaEdit, FaTimes } from 'react-icons/fa';

import { ReduxState } from '../../../../types';
import { mapDispatchToPropsEmpty } from '../../../Common/utils/reactUtils';
import AceEditor from '../../../Common/AceEditor/BaseEditor';
import URLPreview from './URLPreview';
import { allowedQueriesCollection } from '../../../../metadata/utils';
import Button from '../../../Common/Button';
import { dropRESTEndpoint } from '../../../../metadata/actions';
import _push from '../../Data/push';
import Landing from './Landing';
import { badgeSort } from './utils';
import CollapsibleToggle from '../../../Common/CollapsibleToggle/CollapsibleToggle';

const ListComponent: React.FC<Props> = ({
  restEndpoints,
  queryCollections,
  dispatch,
}) => {
  const allowedQueries = queryCollections?.find(
    collection => collection.name === allowedQueriesCollection
  );

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

  return (
    <div className="pl-md pt-md pr-md">
      <div className="flex">
        <h2 className="text-xl font-bold">REST Endpoints</h2>
      </div>
      <div className="pt-md">
        Create endpoints from GraphQL queries using{' '}
        <Link to="/api/api-explorer">GraphiQL</Link>.
        <div className="w-8/12 mt-sm">
          REST endpoints allow for the creation of a REST interface to your
          saved GraphQL queries and mutations. Endpoints are generated from
          /api/rest/* and inherit the authorization and permission structure
          from your associated GraphQL nodes. (
          <a
            href="https://hasura.io/docs/latest/graphql/core/api-reference/restified.html"
            target="_blank"
            rel="noopener noreferrer"
          >
            Know More
          </a>
          )
        </div>
      </div>

      <div className="mt-md overflow-x-auto border border-gray-300 rounded">
        <table className="min-w-full divide-y divide-gray-200">
          <thead className="bg-gray-50">
            <th className="px-sm py-xs max-w-xs text-left text-sm bg-gray-50 font-semibold text-gray-600 uppercase tracking-wider">
              Details
            </th>
            <th className="px-sm py-xs text-left text-sm bg-gray-50 font-semibold text-gray-600 uppercase tracking-wider">
              Endpoint
            </th>
            <th className="px-sm py-xs text-left text-sm bg-gray-50 font-semibold text-gray-600 uppercase tracking-wider">
              Methods
            </th>
            <th className="px-sm py-xs float-right text-sm bg-gray-50 font-semibold text-gray-600 uppercase tracking-wider">
              Modify
            </th>
          </thead>
          <tbody className="bg-white divide-y divide-gray-200">
            {restEndpoints.map(endpoint => (
              <>
                <tr key={`rest_list_endpoint_${endpoint.name}`}>
                  {/* Details */}
                  <td className="px-sm py-xs max-w-xs align-top">
                    <div className="flex items-center">
                      <Link
                        to={{
                          pathname: `/api/rest/details/${endpoint.name}`,
                          state: {
                            ...endpoint,
                            currentQuery: findQuery(endpoint.name),
                          },
                        }}
                      >
                        <h4>{endpoint.name}</h4>
                      </Link>
                    </div>
                    {endpoint.comment && <p>{endpoint.comment}</p>}
                  </td>

                  {/* Endpoint */}
                  <td className="px-sm py-xs align-top">
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
                        />
                      </CollapsibleToggle>
                    </div>
                  </td>

                  <td className="px-sm py-xs align-top">
                    {badgeSort(endpoint.methods).map(method => (
                      <span className="mr-sm" key={`badge-list-${method}`}>
                        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-sm font-semibold bg-blue-100 text-blue-800">
                          {method}
                        </span>
                      </span>
                    ))}
                  </td>

                  {/* Modify Column */}
                  <td className="px-sm py-xs align-top float-right">
                    <Button
                      size="sm"
                      onClick={onClickDelete(
                        endpoint.name,
                        findQuery(endpoint.name)
                      )}
                      color="white"
                      className="mr-1"
                    >
                      <FaTimes className="mr-1" />
                      Delete
                    </Button>
                    <Button
                      size="sm"
                      onClick={onClickEdit(endpoint.name)}
                      color="white"
                    >
                      <FaEdit className="mr-1" />
                      Edit
                    </Button>
                  </td>
                </tr>
              </>
            ))}
          </tbody>
        </table>
      </div>
    </div>
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
interface Props extends InjectedProps {}
const ConnectedComponent = listComponentConnector(ListComponent);

export default ConnectedComponent;
