import gql from 'graphql-tag';

const fetchFiltersData = gql`
  query fetchFiltersData($projectId: uuid) {
    status: subscription_workers(
      distinct_on: status
      where: { project_id: { _eq: $projectId } }
    ) {
      status
    }
  }
`;

const getSubscriptionWorkers = () => {
  const fetchSubscriptionWorkers = gql`
    query fetchSubscriptionWorkers(
      $limit: Int!
      $offset: Int!
      $args: search_subscription_workers_args!
      $where: poller_workers_bool_exp!
      $orderBy: [poller_workers_order_by!]
    ) {
      search_subscription_workers(
        limit: $limit
        offset: $offset
        args: $args
        where: $where
        order_by: $orderBy
      ) {
        started: min_time
        status
        operation_logs(where: { transport: { _eq: "ws" } }, limit: 1) {
          operation_name
        }
        role: user_role
        session_variables
        no_of_subscribers: total_subscribers
        last_execution_time
        no_of_postgres_queries: execution_batch_size
        worker_id: poller_id
        last_activity_time: max_time
      }
      search_subscription_workers_aggregate(args: $args) {
        aggregate {
          count
        }
      }
    }
  `;
  return fetchSubscriptionWorkers;
};

const fetchSubscriptionWorkerDetail = gql`
  query getLatestSubscriptionWorkerById($projectId: uuid!, $pollerId: uuid!) {
    poller_logs_init(
      where: { project_id: { _eq: $projectId }, poller_id: { _eq: $pollerId } }
      order_by: { time: desc }
      limit: 1
    ) {
      poller_id
      time: last_activity_time
      last_execution_time: total_time
      user_role
      refetch_delay
      batch_size
      execution_batch_size
      total_subscribers
      session_variables
      generated_sql
      operation_logs(where: { transport: { _eq: "ws" } }, limit: 1) {
        operation_id
        operation_name
        variables: query(path: ".variables")
        operation_string: query(path: ".query")
      }
    }
  }
`;

const fetchExecutionTimeOvertime = gql`
  query fetchExecutionTimeOvertime(
    $from_time: timestamptz
    $operation_names: _text
    $poller_ids: _uuid
    $project_ids: _uuid
    $status: _text
    $time_interval: String
    $to_time: timestamptz
  ) {
    search_average_poller_execution_time_over_time(
      args: {
        from_time: $from_time
        operation_names: $operation_names
        poller_ids: $poller_ids
        project_ids: $project_ids
        status: $status
        time_interval: $time_interval
        to_time: $to_time
      }
    ) {
      project_id
      timestamp
      value
    }
  }
`;

const fetchPostgresQueriesOvertime = gql`
  query fetchPostgresQueriesOvertime(
    $from_time: timestamptz
    $operation_names: _text
    $poller_ids: _uuid
    $project_ids: _uuid
    $status: _text
    $time_interval: String
    $to_time: timestamptz
  ) {
    search_total_subscription_postgres_queries_over_time(
      args: {
        from_time: $from_time
        operation_names: $operation_names
        poller_ids: $poller_ids
        project_ids: $project_ids
        status: $status
        time_interval: $time_interval
        to_time: $to_time
      }
      order_by: { timestamp: asc }
    ) {
      project_id
      timestamp
      value
    }
  }
`;

const fetchSubscribersOvertime = gql`
  query fetchSubscribersOvertime(
    $from_time: timestamptz
    $operation_names: _text
    $poller_ids: _uuid
    $project_ids: _uuid
    $status: _text
    $time_interval: String
    $to_time: timestamptz
  ) {
    search_total_subscription_subcribers_over_time(
      args: {
        from_time: $from_time
        operation_names: $operation_names
        poller_ids: $poller_ids
        project_ids: $project_ids
        status: $status
        time_interval: $time_interval
        to_time: $to_time
      }
      order_by: { timestamp: asc }
    ) {
      project_id
      timestamp
      value
    }
  }
`;

export {
  fetchFiltersData,
  getSubscriptionWorkers,
  fetchSubscriptionWorkerDetail,
  fetchExecutionTimeOvertime,
  fetchPostgresQueriesOvertime,
  fetchSubscribersOvertime,
};
