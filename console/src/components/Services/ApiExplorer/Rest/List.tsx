import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { Link } from 'react-router';

import { ReduxState } from '../../../../types';
import { mapDispatchToPropsEmpty } from '../../../Common/utils/reactUtils';
import { Badge } from '../../../UIKit/atoms';
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
        <div className={styles.rest_list_header_know_more}>
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
      <hr />
      {restEndpoints.map(endpoint => (
        <>
          <div
            className={styles.rest_list_item_style}
            key={`rest_list_endpoint_${endpoint.name}`}
          >
            <div className={styles.rest_list_left_content}>
              <div className={styles.rest_list_header}>
                <Link
                  to={{
                    pathname: `/api/rest/details/${endpoint.name}`,
                    state: {
                      ...endpoint,
                      currentQuery: findQuery(endpoint.name),
                    },
                  }}
                >
                  <h4
                    className={`${styles.subheading_text_no_padd} ${styles.display_inline}`}
                  >
                    {endpoint.name}
                  </h4>
                </Link>
                {badgeSort(endpoint.methods).map(method => (
                  <span
                    className={styles.padd_small_left}
                    key={`badge-list-${method}`}
                  >
                    <Badge type={`rest-${method}`} />
                  </span>
                ))}
              </div>
              <URLPreview urlInput={endpoint.url} />
              {endpoint.comment && <p>{endpoint.comment}</p>}
              <CollapsibleToggle title="GraphQL Request" useDefaultTitleStyle>
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
            <div className={styles.rest_list_right_content}>
              <div className={styles.rest_list_action_btns}>
                <Button
                  size="sm"
                  onClick={onClickDelete(
                    endpoint.name,
                    findQuery(endpoint.name)
                  )}
                  color="white"
                  style={{ marginRight: '4px' }}
                >
                  <i className="fa fa-times" style={{ marginRight: '4px' }} />
                  Delete
                </Button>
                <Button
                  size="sm"
                  onClick={onClickEdit(endpoint.name)}
                  color="white"
                >
                  <i className="fa fa-edit" style={{ marginRight: '4px' }} />
                  Edit
                </Button>
              </div>
            </div>
          </div>
          <hr />
        </>
      ))}
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
