import gql from 'graphql-tag';

const fetchOperationType = gql`
  query fetchOperationType {
    operations(distinct_on: type, where: { type: { _neq: "" } }) {
      type: operation_type
    }
  }
`;

const fetchOperationId = gql`
  query fetchOperationId {
    http_query_logs(
      distinct_on: operation_id
      where: { operation_id: { _neq: "" } }
    ) {
      operation_id
    }
  }
`;

const fetchRoles = gql`
  query fetchRoles {
    http_logs(distinct_on: user_role) {
      user_role
    }
  }
`;

const fetchOperationName = gql`
  query fetchOperations {
    operations(distinct_on: operation_name) {
      operation_name
    }
  }
`;

const fetchQueryList = gql`
  query fetchQueryList(
    $limit: Int!
    $offset: Int!
    $orderBy: json
    $fromTime: timestamptz!
    $toTime: timestamptz!
    $operation_id: [String!]
    $operation_name: [String!]
    $operation_type: [String!]
    $transport: [String!]
    $user_role: [String!]
    $client_name: [String!]
    $groupBys: [String!]
    $project_id: [uuid!]
  ) {
    searchUsageMetricsAggregate(
      args: {
        from_time: $fromTime
        to_time: $toTime
        group_by: $groupBys
        operation_types: $operation_type
        operation_ids: $operation_id
        operation_names: $operation_name
        transports: $transport
        project_ids: $project_id
        user_roles: $user_role
        client_names: $client_name
      }
    ) {
      count
    }

    searchUsageMetrics(
      args: {
        from_time: $fromTime
        to_time: $toTime
        group_by: $groupBys
        operation_types: $operation_type
        operation_ids: $operation_id
        operation_names: $operation_name
        project_ids: $project_id
        user_roles: $user_role
        client_names: $client_name
        transports: $transport
        limit: $limit
        offset: $offset
        order_by: $orderBy
      }
    ) {
      operation_id
      operation_name
      operation_type
      client_name
      role: user_role
      request_count
      average_response_size
      average_execution_time
      error_count
      transport
    }
  }
`;

const fetchQueryLogForOperation = gql`
  query fetchQueryLogForOperation($opId: String, $opName: String) {
    operation_logs(
      where: {
        _and: [
          { operation_name: { _eq: $opName } }
          { operation_id: { _eq: $opId } }
        ]
      }
    ) {
      query
    }
  }
`;

const searchUsageOverTime = gql`
  query searchUsageOverTime($args: search_usage_over_time_args!) {
    search_usage_over_time(args: $args, order_by: { timestamp: asc }) {
      timestamp
      error_count
      success_count
    }
  }
`;

export {
  fetchOperationName,
  fetchQueryList,
  searchUsageOverTime,
  fetchOperationType,
  fetchOperationId,
  fetchRoles,
  fetchQueryLogForOperation,
};
