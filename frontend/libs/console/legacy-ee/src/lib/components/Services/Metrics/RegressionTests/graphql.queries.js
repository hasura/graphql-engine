import gql from 'graphql-tag';

export const createTestSuite = gql`
  mutation createTestSuite($data: [test_suites_insert_input!]!) {
    insert_test_suites(objects: $data) {
      affected_rows
      returning {
        id
        name
      }
    }
  }
`;

export const fetchOperationsForTestSuite = gql`
  query fetchOperations(
    $projectId: uuid
    $testSuiteId: uuid
    $limit: Int!
    $offset: Int!
  ) {
    results: test_suite_operations(
      where: {
        project_id: { _eq: $projectId }
        test_suite_id: { _eq: $testSuiteId }
      }
      limit: $limit
      offset: $offset
      order_by: { updated_at: desc }
    ) {
      name: operation_name
      query
      variables
      role: user_vars(path: "x-hasura-role")
      session_variables: user_vars
    }
    results_aggregate: test_suite_operations_aggregate(
      where: {
        project_id: { _eq: $projectId }
        test_suite_id: { _eq: $testSuiteId }
      }
    ) {
      aggregate {
        count
      }
    }
  }
`;

export const addOpsToTestSuite = gql`
  mutation insertTestSuiteOperations(
    $operations: [test_suite_operations_insert_input!]!
  ) {
    insert_test_suite_operations(objects: $operations) {
      affected_rows
    }
  }
`;

export const fetchTestSuites = gql`
  query getTestSuites($projectId: uuid) {
    test_suites(where: { project_id: { _eq: $projectId } }) {
      name
      id
    }
  }
`;

export const deleteOperationsFromTestSuite = gql`
  mutation deleteOperationsFromTestSuite(
    $testSuiteId: uuid
    $operationNames: [String!]
  ) {
    delete_test_suite_operations(
      where: {
        operation_name: { _in: $operationNames }
        test_suite_id: { _eq: $testSuiteId }
      }
    ) {
      affected_rows
    }
  }
`;

export const fetchAllOperations = gql`
  query fetchOperations($projectId: uuid, $limit: Int!, $offset: Int!) {
    results: operations(
      where: {
        project_id: { _eq: $projectId }
        _not: { test_suite_operations: { project_id: { _eq: $projectId } } }
        type: { _neq: "subscription" }
      }
      limit: $limit
      offset: $offset
      order_by: { updated_at: desc }
    ) {
      last_seen: updated_at
      operation_id
      name
      query
      http_logs(
        limit: 1
        order_by: { time: desc }
        where: { level: { _eq: "info" } }
      ) {
        variables: query(path: "variables")
        role: user_role
        session_variables: user_vars
      }
    }
    results_aggregate: operations_aggregate(
      where: {
        project_id: { _eq: $projectId }
        _not: { test_suite_operations: { project_id: { _eq: $projectId } } }
      }
    ) {
      aggregate {
        count
      }
    }
  }
`;

export const insertTestSuiteOperations = gql`
  mutation insertTestSuiteOperations(
    $operations: [test_suite_operations_insert_input!]!
  ) {
    insert_test_suite_operations(objects: $operations) {
      affected_rows
    }
  }
`;

export const createTestRun = gql`
  mutation createTestRun($input: CreateTestRunInput!) {
    createTestRun(inp: $input) {
      id
    }
  }
`;

export const subscribeTestRunDetails = gql`
  subscription subscribeTestRunDetails($testRunId: uuid) {
    test_runs(where: { id: { _eq: $testRunId } }) {
      status
      finished_at
      test_number
      created_at
      updated_at
      test_run_details {
        status
        name: operation_name
        role: user_vars(path: "x-hasura-role")
        message: response
      }
    }
  }
`;

export const fetchTestRunDetailsByName = gql`
  query fetchTestRunDetailsByName($testRunId: uuid, $operationName: String!) {
    test_run_details(
      where: {
        test_run_id: { _eq: $testRunId }
        operation_name: { _eq: $operationName }
      }
    ) {
      response
      query
      user_vars
      variables
      status
    }
  }
`;

export const fetchAllTestRuns = gql`
  query fetchAllTestRuns($testSuiteId: uuid, $limit: Int, $offset: Int) {
    test_runs(
      where: { test_suite_id: { _eq: $testSuiteId } }
      order_by: { created_at: desc }
      limit: $limit
      offset: $offset
    ) {
      id
      created_at
      test_number
      status
      finished_at
      test_run_details {
        status
        name: operation_name
        role: user_vars(path: "x-hasura-role")
        message: response
      }
    }
  }
`;

export const cancelTestRun = gql`
  mutation cancelTestRun($testRunId: uuid) {
    update_test_run_details(
      _set: { status: "canceled" }
      where: {
        test_run_id: { _eq: $testRunId }
        status: { _in: ["queued", "running"] }
      }
    ) {
      affected_rows
    }
  }
`;

export const updateVariables = gql`
  mutation setVariables(
    $opsName: String
    $testSuiteId: uuid
    $variables: jsonb
  ) {
    update_test_suite_operations(
      _set: { variables: $variables }
      where: {
        operation_name: { _eq: $opsName }
        test_suite_id: { _eq: $testSuiteId }
      }
    ) {
      affected_rows
    }
  }
`;

export const fetchRunTestPreview = gql`
  query fetchOperations($projectId: uuid, $testSuiteId: uuid) {
    results: test_suite_operations(
      where: {
        project_id: { _eq: $projectId }
        test_suite_id: { _eq: $testSuiteId }
      }
      order_by: { updated_at: desc }
    ) {
      name: operation_name
      role: user_vars(path: "x-hasura-role")
    }
  }
`;

export const fetchTestSuiteOperationByName = gql`
  query fetchTestSuiteOperationByName($opsName: String, $testSuiteId: uuid) {
    test_suite_operations(
      where: {
        operation_name: { _eq: $opsName }
        test_suite_id: { _eq: $testSuiteId }
      }
    ) {
      query
      user_vars
      variables
    }
  }
`;
