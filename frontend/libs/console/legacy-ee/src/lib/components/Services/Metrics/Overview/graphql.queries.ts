import gql from 'graphql-tag';
const fetchLiveStats = gql`
  subscription GetLiveStats($projectIds: _uuid = "") {
    search_latest_project_metrics(args: { project_ids: $projectIds }) {
      project_id
      warp_threads
      websocket_connections
      active_subscriptions
    }
  }
`;
const fetchApiHealthData = gql`
  query RequestsOverTime(
    $args: search_general_operation_metrics_over_time_args!
  ) {
    search_general_operation_metrics_over_time(
      args: $args
      order_by: { bucket_time: asc }
    ) {
      project_id
      bucket_time
      request_count
      success_count
      average_execution_time
      requests_per_minute
    }
  }
`;
const fetchTop5ErrorRates = gql`
  query FindTop5ErrorRates(
    $from_time: timestamptz
    $project_ids: _uuid
    $to_time: timestamptz
  ) {
    search_operation_name_summaries(
      args: {
        to_time: $to_time
        project_ids: $project_ids
        from_time: $from_time
        group_bys: "{operation_id, operation_name}"
      }
      order_by: { error_rate: desc }
      limit: 5
    ) {
      operation_name
      error_rate
      operation_id
    }
  }
`;
const fetchTop5Requests = gql`
  query FindTop5Requests(
    $from_time: timestamptz = "2021-03-16"
    $project_ids: _uuid = "{15115cd3-dd45-44f6-bb27-50a8a2952c9d}"
    $to_time: timestamptz = "2021-03-17"
  ) {
    search_operation_name_summaries(
      args: {
        from_time: $from_time
        to_time: $to_time
        project_ids: $project_ids
        group_bys: "{operation_id, operation_name}"
      }
      order_by: { requests_per_minute: desc }
      limit: 5
    ) {
      operation_name
      requests_per_minute
      operation_id
    }
  }
`;
const fetchTop5Latencies = gql`
  query FindTop5Latencies(
    $from_time: timestamptz
    $project_ids: _uuid
    $to_time: timestamptz
  ) {
    search_operation_name_summaries(
      args: {
        from_time: $from_time
        to_time: $to_time
        project_ids: $project_ids
        group_bys: "{operation_id, operation_name}"
      }
      order_by: { max_execution_time: desc }
      limit: 5
    ) {
      operation_name
      max_execution_time
      operation_id
    }
  }
`;
export {
  fetchApiHealthData,
  fetchTop5Requests,
  fetchTop5ErrorRates,
  fetchTop5Latencies,
  fetchLiveStats,
};
