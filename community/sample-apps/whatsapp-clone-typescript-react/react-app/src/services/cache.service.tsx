import { defaultDataIdFromObject } from 'apollo-cache-inmemory'
import * as fragments from '../graphql/fragments'
import * as subscriptions from '../graphql/subscriptions'
import * as queries from '../graphql/queries'
import gql from 'graphql-tag'
import {
  ChatsListQuerySubUpdate,
  ChatsListQueryCache,
  MessageAdded,
  UserUpdated,
} from '../graphql/types'
import { useSubscription } from '../polyfills/react-apollo-hooks'

const query = gql`
  query ChatsListQueryCache($userId: Int!) {
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
`;

export const useSubscriptions = (userResult) => {
  useSubscription<ChatsListQuerySubUpdate.Subscription, ChatsListQuerySubUpdate.Variables>(
    subscriptions.chatUpdated, { variables: {userId: userResult.id},
    onSubscriptionData: ({ client, subscriptionData: { chat } }) => {
      let chats
      if(chat && chat.length) {
        try {
          chats = client.readQuery<ChatsListQueryCache.Query, ChatsListQueryCache.Variables>({
            query: query,
            variables: {userId: userResult.id}
          }).chat
        } catch(e) {
          console.error(e)
        }
        if (chats && !chats.some(_chat => _chat.id === chat[0].id)) {
          chats.unshift(chat[0])
          try {
            client.writeQuery<ChatsListQueryCache.Query, ChatsListQueryCache.Variables>({
              query: query,
              variables: {userId: userResult.id},
              data: { chat: chats },
            })
          } catch(e) {
            console.error(e)
          }
        }
      }
    },
  })

  useSubscription<MessageAdded.Subscription>(subscriptions.messageAdded, {
    onSubscriptionData: ({ client, subscriptionData: { message_user } }) => {
      let chatId
      let chats
      if(message_user && message_user.length) {
        chatId = message_user[0].chat_id
        try {
          chats = client.readQuery<ChatsListQueryCache.Query, ChatsListQueryCache.Variables>({
            query: query,
            variables: {userId: userResult.id}
          }).chat
          // find array index of new message's chat id in existing cache
          const chatIndex = chats.findIndex(elem => elem.id === chatId)
          const finalChats = [chats[chatIndex], ...chats.filter((item) => item.id !== chatId)]
          client.writeQuery({
            query: query,
            variables: {userId: userResult.id},
            data: { chat: finalChats },
          })
        } catch(e) {
          console.error(e)
        }

      }
    },
  })

  useSubscription<UserUpdated.Subscription>(subscriptions.userUpdated, {
    onSubscriptionData: ({ client, subscriptionData: { users } }) => {
      if(users && users[0]) {
        client.writeFragment({
          id: defaultDataIdFromObject(users[0]),
          fragment: fragments.user,
          data: users[0],
        })
      }
    },
  })
}
