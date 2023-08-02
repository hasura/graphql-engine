import React, { useRef } from 'react';
import { RouteComponentProps } from 'react-router';
import { connect, ConnectedProps } from 'react-redux';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';

import { Button } from '../../../../new-components/Button';
import BreadCrumb from '../../../Common/Layout/BreadCrumb/BreadCrumb';
import AceEditor from '../../../Common/AceEditor/BaseEditor';
import { Badge } from '../../../UIKit/atoms';
import { Dispatch, ReduxState } from '../../../../types';
import _push from '../../Data/push';
import { badgeSort, getCurrentPageHost, inputStyles } from './utils';
import { LS_KEYS, setLSItem } from '../../../../utils/localStorage';
import LivePreview from './LivePreview';

interface DetailsComponentProps extends InjectedProps {
  location: RouteComponentProps<unknown, unknown>['location'];
}

const DetailsComponent: React.FC<DetailsComponentProps> = ({
  location,
  pushToRoute,
  dataHeaders,
}) => {
  const endpointState = location.state;
  const endpointLocation = useRef(
    `${getCurrentPageHost()}/api/rest/${endpointState.url}`
  );

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
    <Analytics name="RestDetails" {...REDACT_EVERYTHING}>
      <div className="px-md pt-md">
        <div className="">
          <BreadCrumb breadCrumbs={crumbs} />
        </div>
        <div className="flex">
          <h2 className="text-xl font-bold inline-block">
            {endpointState.name}
          </h2>
          <Button
            mode="primary"
            size="md"
            className="ml-md"
            onClick={onClickEdit}
          >
            Edit Endpoint
          </Button>
        </div>
        <div className="mt-md">{endpointState.comment}</div>
        <hr className="my-md" />
        <div className="flex w-full justify-between">
          <div className="w-7/12">
            <div>
              <h4 className="text-base font-bold mb-md">REST Endpoint</h4>
              <input
                className={`${inputStyles} w-full disabled:bg-gray-100`}
                type="text"
                placeholder="Rest endpoint URL"
                value={endpointLocation && endpointLocation.current}
                data-key="name"
                disabled
                required
              />
            </div>
            <h4 className="text-base font-bold pt-md mb-md">
              Methods Available
            </h4>
            <div>
              {badgeSort(endpointState.methods).map(method => (
                <span className="pr-md" key={`badge-details-${method}`}>
                  <Badge type={`rest-${method}`} />
                </span>
              ))}
            </div>
            <hr className="mb-lg mt-md" />
            <div className="flex align-center justify-between mb-md">
              <h4 className="text-base font-bold pt-xs">GraphQL Request</h4>
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
              placeholder="query SampleQuery {}"
              height="300px"
              width="100%"
              mode="graphqlschema"
              readOnly
              setOptions={{ useWorker: false }}
            />
          </div>
          <div className="h-full w-1/2 pl-xl">
            <LivePreview
              endpointState={endpointState}
              pageHost={endpointLocation && endpointLocation.current}
              dataHeaders={dataHeaders}
            />
          </div>
        </div>
      </div>
    </Analytics>
  );
};

const mapDispatchToProps = (dispatch: Dispatch) => ({
  pushToRoute: (link: string) => dispatch(_push(link)),
});
const mapStateToProps = (state: ReduxState) => ({
  dataHeaders: state?.apiexplorer?.displayedApi?.request?.headers ?? [],
});
const detailsComponentConnector = connect(mapStateToProps, mapDispatchToProps);
type InjectedProps = ConnectedProps<typeof detailsComponentConnector>;

export default detailsComponentConnector(DetailsComponent);
