import React from 'react';
import { RouteComponentProps } from 'react-router';
import { connect, ConnectedProps } from 'react-redux';

import BreadCrumb from '../../../Common/Layout/BreadCrumb/BreadCrumb';
import AceEditor from '../../../Common/AceEditor/BaseEditor';
import Button from '../../../Common/Button/Button';
import { Badge } from '../../../UIKit/atoms';
import { Dispatch } from '../../../../types';
import _push from '../../Data/push';
import { badgeSort, getCurrentPageHost } from './utils';
import { LS_KEYS, setLSItem } from '../../../../utils/localStorage';

import styles from './RESTStyles.scss';

interface DetailsComponentProps extends InjectedProps {
  location: RouteComponentProps<unknown, unknown>['location'];
}

const DetailsComponent: React.FC<DetailsComponentProps> = ({
  location,
  pushToRoute,
}) => {
  const endpointState = location.state;
  const endText = getCurrentPageHost();

  const crumbs = [
    {
      title: 'REST Endpoints',
      url: `/api/rest`,
    },
    {
      title: endpointState.name,
      url: '',
    },
  ];

  const onClickEdit = () => {
    pushToRoute(`/api/rest/edit/${endpointState.name}`);
  };

  const onClickOpenInGQL = (query: string) => () => {
    setLSItem(LS_KEYS.graphiqlQuery, query);
    pushToRoute('/api/api-explorer');
  };

  return (
    <>
      <div
        className={`container-fluid ${styles.rest_add_padding_left} ${styles.padd_top}`}
      >
        <div className={styles.subHeader}>
          <BreadCrumb breadCrumbs={crumbs} />
        </div>
        <div className={styles.display_flex}>
          <h2 className={`${styles.headerText} ${styles.display_inline}`}>
            {endpointState.name}
          </h2>
          <Button
            color="yellow"
            size="xs"
            className={styles.add_mar_left}
            onClick={onClickEdit}
          >
            Edit Endpoint
          </Button>
        </div>
        <div className={styles.add_mar_top}>{endpointState.comment}</div>
        <hr />
        <div>
          <h4 className={`${styles.subheading_text} ${styles.display_inline}`}>
            REST Endpoint
          </h4>
          <input
            className="form-control"
            type="text"
            placeholder="Rest endpoint URL"
            value={`${endText}/api/rest/${endpointState.url}`}
            data-key="name"
            disabled
            required
          />
        </div>
        <div>
          <h4
            className={`${styles.subheading_text} ${styles.display_inline} ${styles.padd_top}`}
          >
            Methods Available
          </h4>
          <div>
            {badgeSort(endpointState.methods).map(method => (
              <span
                className={`${styles.headerBadge} ${styles.padd_right}`}
                key={`badge-details-${method}`}
              >
                <Badge type={`rest-${method}`} />
              </span>
            ))}
          </div>
        </div>
        <hr />
        <div className={styles.gql_header_details}>
          <h4 className={`${styles.subheading_text} ${styles.display_inline}`}>
            GraphQL Request
          </h4>
          <Button
            size="sm"
            color="white"
            onClick={onClickOpenInGQL(endpointState.currentQuery)}
          >
            Open in GraphiQL
          </Button>
        </div>
        <AceEditor
          name="query-viewer"
          value={endpointState.currentQuery}
          placeholder={`query SampleQuery {}`}
          height="300px"
          mode="graphqlschema"
          readOnly
        />
        <hr />
      </div>
    </>
  );
};

const mapDispatchToProps = (dispatch: Dispatch) => ({
  pushToRoute: (link: string) => dispatch(_push(link)),
});
const detailsComponentConnector = connect(null, mapDispatchToProps);
type InjectedProps = ConnectedProps<typeof detailsComponentConnector>;

export default detailsComponentConnector(DetailsComponent);
