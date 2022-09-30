import gql from 'graphql-tag';

const fetchFiltersData = gql`
  query fetchFiltersDataForErrors($projectId: uuid) {
    operation_types: operations(
      distinct_on: type
      where: { project_id: { _eq: $projectId } }
    ) {
      operation_type: type
    }
    user_roles: http_logs(
      distinct_on: user_role
      where: { project_id: { _eq: $projectId } }
    ) {
      user_role
    }
    error_codes: http_logs(
      distinct_on: error_code
      where: { project_id: { _eq: $projectId } }
    ) {
      error_code
    }
    client_names: http_logs(
      distinct_on: client_name
      where: { project_id: { _eq: $projectId } }
    ) {
      client_name
    }
  }
`;

const fetchOperationTypes = gql`
  query fetchOperationTypesForErrors {
    operations(distinct_on: type) {
      operation_type: type
    }
  }
`;

const fetchRoles = gql`
  query fetchRolesForErrors {
    http_logs(distinct_on: user_role) {
      user_role
    }
  }
`;

const fetchClientNames = gql`
  query fetchClientNamesForErrors {
    http_logs(distinct_on: client_name) {
      client_name
    }
  }
`;

const fetchErrorCodes = gql`
  query fetchErrorCodes {
    http_logs(distinct_on: error_code) {
      error_code
    }
  }
`;

const fetchErrorOperations = gql`
  query fetchErrorOperations {
    frequent_errors(distinct_on: operation_name) {
      operation_name
    }
  }
`;

const getFetchErrorsWithGroupBy = () => {
  const fetchGroupedErrors = gql`
    query fetchGroupedErrors(
      $limit: Int!
      $offset: Int!
      $orderBy: json
      $fromTime: timestamptz!
      $toTime: timestamptz!
      $error_code: [String!]
      $operation_id: [String!]
      $operation_name: [String!]
      $operation_type: [String!]
      $user_role: [String!]
      $client_name: [String!]
      $transport: [String!]
      $groupBys: [String!]!
      $project_id: [uuid!]
    ) {
      searchErrorMetrics(
        args: {
          from_time: $fromTime
          to_time: $toTime
          group_by: $groupBys
          operation_types: $operation_type
          operation_ids: $operation_id
          operation_names: $operation_name
          project_ids: $project_id
          user_roles: $user_role
          error_codes: $error_code
          client_names: $client_name
          transports: $transport
          order_by: $orderBy
          limit: $limit
          offset: $offset
        }
      ) {
        operation_id
        operation_name
        operation_type
        error_code
        role: user_role
        request_count
        error_count
        client_name
        transport
      }
      searchErrorMetricsAggregate(
        args: {
          from_time: $fromTime
          to_time: $toTime
          group_by: $groupBys
          operation_types: $operation_type
          operation_ids: $operation_id
          operation_names: $operation_name
          project_ids: $project_id
          user_roles: $user_role
          error_codes: $error_code
          client_names: $client_name
          transports: $transport
        }
      ) {
        count
      }
    }
  `;
  return fetchGroupedErrors;
};

const searchErrorsOverTime = gql`
  query searchErrorsOverTime(
    $fromTime: timestamptz!
    $toTime: timestamptz!
    $error_code: _text
    $operation_id: _text
    $operation_name: _text
    $user_role: _text
    $transport: _text
    $timeInterval: String
    $client_name: _text
    $project_id: _uuid
  ) {
    search_errors_over_time(
      order_by: { timestamp: asc }
      args: {
        from_time: $fromTime
        to_time: $toTime
        operation_ids: $operation_id
        operation_names: $operation_name
        project_ids: $project_id
        user_roles: $user_role
        error_codes: $error_code
        time_interval: $timeInterval
        client_names: $client_name
        transports: $transport
      }
    ) {
      timestamp
      value
    }
  }
`;

export {
  fetchErrorOperations,
  fetchErrorCodes,
  fetchOperationTypes,
  fetchRoles,
  fetchClientNames,
  searchErrorsOverTime,
  getFetchErrorsWithGroupBy,
  fetchFiltersData,
};
