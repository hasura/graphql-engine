import gql from 'graphql-tag'
import * as fragments from '../fragments'

export default gql `
  query ChatsUsers {
    chat_users {
      chat {
	    ...chat
      }
      user {
      	...user
      }
    }
  }
  ${fragments.chat}
`
