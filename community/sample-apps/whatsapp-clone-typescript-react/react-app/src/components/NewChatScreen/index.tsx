import { defaultDataIdFromObject } from 'apollo-cache-inmemory'
import gql from 'graphql-tag'
import * as React from 'react'
import { Suspense } from 'react'
import { useQuery, useMutation } from 'react-apollo-hooks'
import { RouteComponentProps } from 'react-router-dom'
import styled from 'styled-components'
import { time as uniqid } from 'uniqid'
import * as fragments from '../../graphql/fragments'
import * as queries from '../../graphql/queries'
import { NewChatScreenMutation, Chats, Chat } from '../../graphql/types'
import { useMe } from '../../services/auth.service'
import Navbar from '../Navbar'
import UsersList from '../UsersList'
import NewChatNavbar from './NewChatNavbar'
import NewGroupButton from './NewGroupButton'

const Style = styled.div`
  .UsersList {
    height: calc(100% - 56px);
  }

  .NewChatScreen-users-list {
    height: calc(100% - 56px);
    overflow-y: overlay;
  }
`

const mutation = gql`
  mutation NewChatScreenMutation($userId: Int!,$currentUserId: Int!) {
    insert_chat(objects: [{
      owner_id: null,
      users: {
        data: [
          {user_id: $userId},
          {user_id: $currentUserId}
        ]
      }
    
    }]) {
      affected_rows
      returning {
        ...chat
      }
    }
  }
  ${fragments.chat}
`;

export default ({ history }: RouteComponentProps) => {
  const me = useMe();

  const addChat = useMutation<NewChatScreenMutation.Mutation, NewChatScreenMutation.Variables>(
    mutation,
    {
      update: (client, { data: { insert_chat } }) => {
        try {
          client.writeFragment<Chat.Fragment>({
            id: defaultDataIdFromObject(insert_chat.returning[0]),
            fragment: fragments.chat,
            fragmentName: 'chat',
            data: insert_chat.returning[0],
          })
        } catch(e) {
          console.error(e)
        }

        let chats
        try {
          chats = client.readQuery<Chats.Query>({
            query: queries.chats,
          })
        } catch (e) {
          console.error(e);
        }

        if (chats && chats.length && !chats.some(chat => chat.id === insert_chat.returning[0].id)) {
          // move new chat to first
          chats.unshift(insert_chat.returning[0])

          client.writeQuery<Chats.Query>({
            query: queries.chats,
            data: { chat: chats },
          })
        }
      },
    },
  );

  const onUserPick = user => {
    const selectedUserId = user.id;
    if(user.isExisting) {
      history.push(`/chats/${user.chat_id}`)
    } else {
      addChat({
        variables: {
          userId: user.id,
          currentUserId: me.id
        },
      })
      .then(({ data: { insert_chat } }) => {
        history.push(`/chats/${insert_chat.returning[0].id}`)
      })
    }

  }

  return (
    <Style className="NewChatScreen Screen">
      <Navbar>
        <NewChatNavbar history={history} />
      </Navbar>
      <div className="NewChatScreen-users-list">
        <NewGroupButton history={history} />
        <Suspense fallback={null}>
          <UsersList onUserPick={onUserPick} />
        </Suspense>
      </div>
    </Style>
  )
}
