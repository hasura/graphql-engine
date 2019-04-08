import gql from 'graphql-tag'
import * as fragments from '../fragments'

export default gql `
  subscription ChatsListQuerySubUpdate($userId: Int!) {
    chat(order_by:[{messages_aggregate:{max:{created_at:desc}}}]) {
      ...chat
      users(where:{user_id:{_neq:$userId}}) {
        user {
          ...user
        }
      }
    }
  }
  ${fragments.chat}
  ${fragments.user}
`
