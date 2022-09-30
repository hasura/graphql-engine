import gql from 'graphql-tag';

const fetchFiltersData = gql`
  query fetchFiltersData($projectId: uuid) {
    websocket_id: websocket_status(
      distinct_on: websocket_id
      where: { project_id: { _eq: $projectId } }
    ) {
      websocket_id
    }
    user_roles: websocket_status(
      distinct_on: user_role
      where: { project_id: { _eq: $projectId } }
    ) {
      user_role
    }
    client_names: websocket_status(
      distinct_on: client_name
      where: { project_id: { _eq: $projectId } }
    ) {
      client_name
    }
  }
`;

const getFetchWebsocketsWithGroupBy = () => {
  const fetchGroupedWebsockets = gql`
    query fetchGroupedWebsockets(
      $limit: Int!
      $offset: Int!
      $orderBy: json
      $fromTime: timestamptz!
      $toTime: timestamptz!
      $groupBys: [String!]!
      $client_name: [String!]
      $user_role: [String!]
    ) {
      searchWebsocketMetrics(
        args: {
          group_by: $groupBys
          from_time: $fromTime
          to_time: $toTime
          limit: $limit
          offset: $offset
          order_by: $orderBy
          client_names: $client_name
          user_roles: $user_role
        }
      ) {
        client_name
        average_connection_duration
        current_open_connections
        error_count
        running_subscriptions
        project_id
        instance_uid
        total_open_attempts
        role: user_role
      }
      searchWebsocketMetricsAggregate(
        args: {
          group_by: $groupBys
          from_time: $fromTime
          to_time: $toTime
          limit: $limit
          offset: $offset
          order_by: $orderBy
          client_names: $client_name
          user_roles: $user_role
        }
      ) {
        count
      }
    }
  `;
  return fetchGroupedWebsockets;
};

const getFetchWebsocketsStatus = () => {
  const fetchWebsocketStatus = gql`
    query fetchWebsocketStatus(
      $limit: Int!
      $offset: Int!
      $where: websocket_status_bool_exp!
      $orderBy: [websocket_status_order_by!]
    ) {
      websocket_status_aggregate(where: $where) {
        aggregate {
          count
        }
      }
      websocket_status(
        limit: $limit
        offset: $offset
        where: $where
        order_by: $orderBy
      ) {
        time: start_time
        success: is_error
        status: event_type
        open_subscriptions
        websocket_id
        client_name
        role: user_role
        error: error_code
        session_variables: user_vars
        instance_id: instance_uid
      }
    }
  `;
  return fetchWebsocketStatus;
};

export {
  fetchFiltersData,
  getFetchWebsocketsWithGroupBy,
  getFetchWebsocketsStatus,
};
