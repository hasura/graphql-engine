import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { Link } from 'react-router';

import { ReduxState } from '../../../../types';
import { mapDispatchToPropsEmpty } from '../../../Common/utils/reactUtils';
import AceEditor from '../../../Common/AceEditor/BaseEditor';
import URLPreview from './Create/URLPreview';
import { allowedQueriesCollection } from '../../../../metadata/utils';
import Button from '../../../Common/Button';
import { dropRESTEndpoint } from '../../../../metadata/actions';
import _push from '../../Data/push';
import Landing from './Landing';
import { badgeSort } from './utils';
import CollapsibleToggle from '../../../Common/CollapsibleToggle/CollapsibleToggle';

import styles from './RESTStyles.scss';

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
    <div
      className={`container-fluid ${styles.rest_add_padding_left} ${styles.padd_top}`}
    >
      <div className={`${styles.display_flex} ${styles.marginBottom}`}>
        <h2 className={`${styles.headerText} ${styles.display_inline}`}>
          REST Endpoints
        </h2>
      </div>
      <div className={`${styles.subHeader} ${styles.padd_top}`}>
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
            <th className="px-md py-sm max-w-xs text-left text-sm bg-gray-50 font-semibold text-gray-600 uppercase tracking-wider">
              Details
            </th>
            <th className="px-md py-sm text-left text-sm bg-gray-50 font-semibold text-gray-600 uppercase tracking-wider">
              Endpoint
            </th>
            <th className="px-md py-sm text-left text-sm bg-gray-50 font-semibold text-gray-600 uppercase tracking-wider">
              Methods
            </th>
            <th className="px-md py-sm text-right text-sm bg-gray-50 font-semibold text-gray-600 uppercase tracking-wider">
              Modify
            </th>
          </thead>
          <tbody className="bg-white divide-y divide-gray-200">
            {restEndpoints.map(endpoint => (
              <>
                <tr key={`rest_list_endpoint_${endpoint.name}`}>
                  {/* Details */}
                  <td className="px-md py-sm max-w-xs align-top">
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
                  <td className="px-md py-sm align-top">
                    <div className={styles.rest_list_left_content}>
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

                  <td className="px-md py-sm align-top">
                    {badgeSort(endpoint.methods).map(method => (
                      <span className="mr-sm" key={`badge-list-${method}`}>
                        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-sm font-semibold bg-blue-100 text-blue-800">
                          {method}
                        </span>
                      </span>
                    ))}
                  </td>

                  {/* Modify Column */}
                  <td className="px-md py-sm align-top text-right">
                    <Button
                      size="sm"
                      onClick={onClickDelete(
                        endpoint.name,
                        findQuery(endpoint.name)
                      )}
                      color="white"
                      style={{ marginRight: '4px' }}
                    >
                      <i
                        className="fa fa-times"
                        style={{ marginRight: '4px' }}
                      />
                      Delete
                    </Button>
                    <Button
                      size="sm"
                      onClick={onClickEdit(endpoint.name)}
                      color="white"
                    >
                      <i
                        className="fa fa-edit"
                        style={{ marginRight: '4px' }}
                      />
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
