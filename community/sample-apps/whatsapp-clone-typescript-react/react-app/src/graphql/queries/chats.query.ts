import gql from 'graphql-tag'
import * as fragments from '../fragments'

export default gql `
  query Chats {
    chat {
      ...chat
      messages {
      	...message
      }
    }
  }
  ${fragments.chat}
  ${fragments.message}
`
