import gql from 'graphql-tag'
import message from './message.fragment'

export default gql `
  fragment chat on chat {
    id
    name
    picture
    owner_id
    created_at
    messages(order_by:[{created_at: asc}]) {
      ...message
    }
  }
  ${message}
`
