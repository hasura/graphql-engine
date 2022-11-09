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

const addToOperationGroup = gql`
  mutation addToOperationGroup(
    $insertObj: [operation_groups_operations_insert_input!]!
  ) {
    insert_operation_groups_operations(objects: $insertObj) {
      affected_rows
      returning {
        operation_group_name
        operation_name
        query
      }
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

const fetchHttpOperationById = gql`
  query fetchHttpOperationById(
    $projectId: uuid!
    $requestId: String!
    $time: timestamptz
  ) {
    operations: operation_logs(
      where: {
        project_id: { _eq: $projectId }
        request_id: { _eq: $requestId }
        time: { _eq: $time }
      }
      limit: 1
    ) {
      operation_id
      time
      operation_name
      client_name
      request_id
      request_size
      server_client_id
      user_role
      execution_time
      response_size
      error
      query
      user_vars
      generated_sql
      request_headers: http_info(path: "request_headers")
      operation_type
      null_response_logs(where: { transport: { _eq: "http" } }, limit: 1) {
        nulls
        empty_arrays
      }
    }
  }
`;

const fetchWsOperationById = gql`
  query fetcWsOperationById($projectId: uuid!, $requestId: String!) {
    operations: operation_logs(
      where: {
        project_id: { _eq: $projectId }
        request_id: { _eq: $requestId }
      }
      limit: 1
    ) {
      operation_id
      time
      operation_name
      client_name
      request_id
      server_client_id
      user_role
      execution_time
      response_size
      request_size
      error
      query
      user_vars
      generated_sql
      request_headers: headers
      websocket_id
      ws_operation_id
      operation_type
      status
      null_response_logs(
        where: { transport: { _eq: "ws" } }
        limit: 1
        order_by: { time: desc }
      ) {
        nulls
        empty_arrays
      }
    }
  }
`;

const fetchOperationType = gql`
  query fetchOperationType {
    operations(distinct_on: type) {
      operation_type: type
    }
  }
`;

const fetchOperationId = gql`
  query fetchOperationId {
    operation_logs(
      distinct_on: operation_id
      where: { operation_id: { _neq: "" } }
    ) {
      operation_id
    }
  }
`;

const fetchRoles = gql`
  query fetchRoles {
    operation_logs(distinct_on: user_role) {
      user_role
    }
  }
`;

const fetchOperationNames = gql`
  query fetchOperations {
    operations(distinct_on: operation_name) {
      operation_name
    }
  }
`;

const fetchErrorCodes = gql`
  query fetchErrorCodes {
    operation_logs(distinct_on: error_code) {
      error_code
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
  fetchErrorCodes,
  fetchOperationNames,
  fetchRoles,
  fetchOperationById,
  fetchOperationType,
  fetchOperationId,
  addToOperationGroup,
  fetchFiltersData,
  fetchHttpOperationById,
  fetchWsOperationById,
  getProjectConfigs,
  fetchSpansByRequestId,
};
