import gql from 'graphql-tag';

export const GetQuery = gql`
  query GetQuery {
    todos {
      id
      text
      is_completed
      created_at
      updated_at
      is_public
      user_id
    }
  }
`;

export const AddMutation = gql`
  mutation AddMutation($objects: [todos_insert_input!]!) {
    insert_todos(objects: $objects) {
      returning {
        id
        text
        is_completed
        created_at
        updated_at
        is_public
        user_id
      }
    }
  }
`;

export const UpdateMutation = gql`
  mutation UpdateMutation($where: todos_bool_exp!, $set: todos_set_input!) {
    update_todos(where: $where, _set: $set) {
      affected_rows
      returning {
        id
        text
        is_completed
        created_at
        updated_at
        is_public
        user_id
      }
    }
  }
`;

export const DeleteMutation = gql`
  mutation DeleteMutation($where: todos_bool_exp!) {
    delete_todos(where: $where) {
      affected_rows
      returning {
        id
      }
    }
  }
`;

export const AddUserMutation = gql`
  mutation AddUserMutation($objects: [users_insert_input!]!) {
    insert_users(objects: $objects) {
      returning {
        id
        name
        created_at
        last_seen
      }
    }
  }
`;

export const UpdateUserMutation = gql`
  mutation UpdateUserLastSeenMutation($where: users_bool_exp!, $set: users_set_input!) {
    update_users(where: $where, _set: $set) {
      affected_rows
      returning {
        last_seen
      }
    }
  }
`;
