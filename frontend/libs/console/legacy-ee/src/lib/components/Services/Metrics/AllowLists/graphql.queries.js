import gql from 'graphql-tag';

export const fetchUserProjects = gql`
  query fetchUserProjectsForAllowLists {
    privilegedProjects(where: { privileges: "admin" }) {
      id
      name
    }
  }
`;

export const fetchAllRemoteAllowedOperations = gql`
  query fetchAllRemoteAllowedOperationsForAllowLists(
    $projectId: uuid
    $groupName: String!
  ) {
    results: operation_groups_operations(
      where: {
        operation_group_name: { _eq: $groupName }
        project_id: { _eq: $projectId }
      }
    ) {
      name: operation_name
      operation_id
      query
    }
  }
`;

/*
export const fetchRemoteOperations = gql`
  query fetchAllowedOperations(
    $remoteProjectId: uuid
    $projectId: uuid
    $groupName: String!
    $limit: Int!
    $offset: Int!
    $orderBy: [operation_groups_operations_order_by!]
  ) {
    results: operation_groups_operations(
      where: {
        operation_group_name: { _eq: $groupName }
        project_id: { _eq: $remoteProjectId }
        _not: { sibling_projects: { project_id: { _eq: $projectId } } }
      }
      limit: $limit
      offset: $offset
      order_by: $orderBy
    ) {
      name: operation_name
      operation_id
      query
    }
    results_aggregate: operation_groups_operations_aggregate(
      where: {
        operation_group_name: { _eq: $groupName }
        project_id: { _eq: $remoteProjectId }
        _not: { sibling_projects: { project_id: { _eq: $projectId } } }
      }
    ) {
      aggregate {
        count
      }
    }
  }
`;
*/

export const fetchRemoteOperations = gql`
  query fetchRemoteOperationsForAllowList(
    $remoteProjectId: uuid
    $projectId: uuid
    $groupName: String!
    $limit: Int!
    $offset: Int!
    $orderBy: [operations_order_by!]
  ) {
    results: operations(
      where: {
        project_id: { _eq: $remoteProjectId }
        _not: {
          groups: { sibling_projects: { project_id: { _eq: $projectId } } }
        }
      }
      limit: $limit
      offset: $offset
      order_by: $orderBy
    ) {
      last_seen: updated_at
      name
      query
      operation_id
      operation_groups_operations(
        where: { operation_group_name: { _eq: $groupName } }
      ) {
        operation_group_name
        id
      }
    }
    results_aggregate: operations_aggregate(
      where: {
        project_id: { _eq: $remoteProjectId }
        _not: {
          groups: { sibling_projects: { project_id: { _eq: $projectId } } }
        }
      }
    ) {
      aggregate {
        count
      }
    }
  }
`;

export const fetchOperations = gql`
  query fetchOperationsForAllowList(
    $projectId: uuid
    $groupName: String!
    $limit: Int!
    $offset: Int!
    $orderBy: [operations_order_by!]
  ) {
    results: operations(
      where: {
        project_id: { _eq: $projectId }
        _not: { operation_groups_operations: {} }
      }
      limit: $limit
      offset: $offset
      order_by: $orderBy
    ) {
      last_seen: updated_at
      name
      query
      operation_id
      operation_groups_operations(
        where: { operation_group_name: { _eq: $groupName } }
      ) {
        operation_group_name
        id
      }
    }
    results_aggregate: operations_aggregate(
      where: {
        project_id: { _eq: $projectId }
        _not: { operation_groups_operations: {} }
      }
    ) {
      aggregate {
        count
      }
    }
  }
`;

export const fetchOperationGroups = gql`
  query fetchOperationGroupsForAllowList(
    $limit: Int!
    $offset: Int!
    $where: operation_groups_bool_exp!
    $orderBy: [operation_groups_order_by!]
  ) {
    operation_groups(
      limit: $limit
      offset: $offset
      where: $where
      order_by: $orderBy
    ) {
      name
      project_id
      created_at
      created_by
    }
  }
`;

export const fetchOperationGroup = gql`
  query fetchOperationGroupForAllowList(
    $projectId: uuid!
    $groupName: String!
  ) {
    operation_groups(
      where: { name: { _eq: $groupName }, project_id: { _eq: $projectId } }
    ) {
      name
    }
  }
`;

export const createOperationGroup = gql`
  mutation createOperationGroupForAllowList(
    $operationInfo: [operation_groups_insert_input!]!
  ) {
    insert_operation_groups(objects: $operationInfo) {
      affected_rows
    }
  }
`;

export const syncOperations = gql`
  mutation syncOperationsForAllowList($name: String!, $projectId: uuid!) {
    syncOperationGroup(where: { name: $name, project_id: $projectId }) {
      message
    }
  }
`;

export const exportAsAllowList = gql`
  query exportAllowListForAllowList($name: String!, $projectId: uuid!) {
    exportAllowList(where: { name: $name, project_id: $projectId }) {
      args
      type
    }
  }
`;
