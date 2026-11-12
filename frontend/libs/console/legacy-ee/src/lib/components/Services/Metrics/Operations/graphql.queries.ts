import gql from 'graphql-tag';

const fetchFiltersData = gql`
  query fetchFiltersDataForOperations($projectId: uuid) {
    operation_types: operations(
      distinct_on: type
      where: { project_id: { _eq: $projectId } }
    ) {
      operation_type: type
    }
    user_roles: operation_logs(
      distinct_on: user_role
      where: { project_id: { _eq: $projectId } }
    ) {
      user_role
    }
    error_codes: operation_logs(
      distinct_on: error_code
      where: { project_id: { _eq: $projectId } }
    ) {
      error_code
    }
    client_names: operation_logs(
      distinct_on: client_name
      where: { project_id: { _eq: $projectId } }
    ) {
      client_name
    }
    operation_groups: operation_groups {
      operation_group_name: name
    }
  }
`;
const fetchOperationById = gql`
  query fetchOperationById($projectId: uuid!, $requestId: String!) {
    operations: operation_logs(
      where: {
        project_id: { _eq: $projectId }
        request_id: { _eq: $requestId }
      }
      limit: 1
    ) {
      time
      request_id
      operation_id
      operation_name
      client_name
      user_role
      execution_time
      request_size
      response_size
      request_size
      error
      query
      user_vars
      transport
      request_headers
      transport
      websocket_id
      ws_operation_id
      kind
      request_mode
    }
  }
`;
const fetchOperations = gql`
  query fetchOperations(
    $limit: Int!
    $offset: Int!
    $where: operation_logs_bool_exp!
    $orderBy: [operation_logs_order_by!]
  ) {
    operation_logs(
      limit: $limit
      offset: $offset
      where: $where
      order_by: $orderBy
    ) {
      time
      success: is_error
      operation_name
      operation_type
      transport
      client_name
      role: user_role
      execution_time
      response_size
      operation_id
      request_id
    }
    operation_groups {
      name
      project_id
    }
  }
`;

const getProjectConfigs = gql`
  query getProjectConfigs($projectId: uuid!) {
    project_configs(where: { project_id: { _eq: $projectId } }) {
      analyze_query_variables
      analyze_response_body
    }
  }
`;

const fetchSpansByRequestId = gql`
  query fetchSpansByRequestId(
    $projectId: uuid!
    $requestId: String!
    $fromTime: timestamptz
    $toTime: timestamptz
  ) {
    tracing_logs(
      where: {
        project_id: { _eq: $projectId }
        tracing_logs: {
          parent_id: { _is_null: true }
          request_id: { _eq: $requestId }
          time: { _gte: $fromTime, _lte: $toTime }
        }
        time: { _gte: $fromTime, _lte: $toTime }
      }
      order_by: { start: asc }
    ) {
      id
      name
      parent_id
      span_id
      time
      duration
      start
      meta: detail(path: "span.meta")
    }
  }
`;
export {
  fetchOperations,
  fetchOperationById,
  fetchFiltersData,
  getProjectConfigs,
  fetchSpansByRequestId,
};
