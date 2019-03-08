import { defaultDataIdFromObject } from 'apollo-cache-inmemory'
import * as fragments from '../graphql/fragments'
import * as subscriptions from '../graphql/subscriptions'
import * as queries from '../graphql/queries'
import gql from 'graphql-tag'
import {
  ChatsListQuerySub,
  ChatsListQuerySubUpdate,
  ChatsListQueryLocal,
  Message,
  UserUpdated,
} from '../graphql/types'
import { useSubscription } from '../polyfills/react-apollo-hooks'

const query = gql`
  query ChatsListQueryLocal($userId: Int!) {
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
  useSubscription<ChatsListQuerySub.Subscription, ChatsListQuerySub.Variables>(
    subscriptions.chatAdded, { variables: {userId: userResult.id},
    onSubscriptionData: ({ client, subscriptionData: { chat } }) => {
      /*
      if(chat && chat.length) {
        client.writeFragment({
          id: defaultDataIdFromObject(chat[0]),
          fragment: fragments.chat,
          fragmentName: 'chat',
          data: chat[0],
        })

        let chats
        try {
          chats = client.readQuery<any>({
            query: queries.chats,
          }).chat
        } catch (e) {}
        console.log(queries.chats);
        console.log(chats);

        if (chats && !chats.some(_chat => _chat.id === chat[0].id)) {
          chats.unshift(chat[0])

          client.writeQuery({
            query: queries.chats,
            data: { chat: chats },
          })
        }
      }
      */
    },
  })

  useSubscription<ChatsListQuerySubUpdate.Subscription, ChatsListQuerySubUpdate.Variables>(
    subscriptions.chatUpdated, { variables: {userId: userResult.id},
    onSubscriptionData: ({ client, subscriptionData: { chat } }) => {
      /*
      if(chat && chat[0]) {
        client.writeFragment({
          id: defaultDataIdFromObject(chat[0]),
          fragment: fragments.chat,
          fragmentName: 'chat',
          data: chat[0],
        })
      }
      let chats
      try {
        chats = client.readQuery<any>({
          query: queries.chats,
          variables: {}
        })
      } catch (e) {
        console.log(e);
      }
      console.log(queries.chats);
      console.log(chats);
      */
      let chats
      if(chat && chat.length) {
        try {
          chats = client.readQuery<ChatsListQueryLocal.Query, ChatsListQueryLocal.Variables>({
            query: query,
            variables: {userId: userResult.id}
          }).chat
        } catch(e) {
          console.error(e)
        }
        if (chats && !chats.some(_chat => _chat.id === chat[0].id)) {
          chats.unshift(chat[0])
          try {
            client.writeQuery<ChatsListQueryLocal.Query, ChatsListQueryLocal.Variables>({
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

  useSubscription<Message.Subscription>(subscriptions.messageAdded, {
    onSubscriptionData: ({ client, subscriptionData: { message } }) => {
      let chatId
      let chats
      if(message && message.length) {
        chatId = message[0].chat_id
        try {
          chats = client.readQuery<ChatsListQueryLocal.Query, ChatsListQueryLocal.Variables>({
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

  /*
  useSubscription<any>(subscriptions.userAdded, {
    // onSubscriptionData: ({ client, subscriptionData: { userAdded } }) => {
    onSubscriptionData: ({ client, subscriptionData: { users } }) => {
      console.log('SUBSCRIPTION NEW USER')
      if(users && users[0]) {
        client.writeFragment({
          id: defaultDataIdFromObject(users[0]),
          fragment: fragments.user,
          data: users[0],
        })

        let finalUsers
        try {
          finalUsers = client.readQuery<any>({
            query: queries.users,
          }).users
        } catch (e) {
          console.log(e);
        }
        console.log(finalUsers);

        if (finalUsers && !finalUsers.some(_user => _user.id === users[0].id)) {
          finalUsers.push(users[0])

          client.writeQuery({
            query: queries.users,
            data: { users: finalUsers },
          })
        }
      }
    },
  })
  */

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
