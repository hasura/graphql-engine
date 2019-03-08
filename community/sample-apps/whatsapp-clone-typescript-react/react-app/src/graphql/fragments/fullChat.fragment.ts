import gql from 'graphql-tag'
import chat from './chat.fragment'
import message from './message.fragment'

export default gql `
  fragment FullChat on chat {
    ...chat
    messages(order_by: [{created_at: asc}]) {
      ...message
    }
  }
  ${chat}
  ${message}
`
