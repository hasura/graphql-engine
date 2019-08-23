import gql from 'graphql-tag'

export default gql`
  fragment messageUser on message_user {
    id
    chat_id
    sender {
      id
      name
    }
    content
    created_at
  }
`
