import Button from '@material-ui/core/Button'
import ArrowRightIcon from '@material-ui/icons/ArrowRightAlt'
import { defaultDataIdFromObject } from 'apollo-cache-inmemory'
import gql from 'graphql-tag'
import { History } from 'history'
import * as React from 'react'
import { useMutation } from 'react-apollo-hooks'
import styled from 'styled-components'
import { time as uniqid } from 'uniqid'
import * as fragments from '../../graphql/fragments'
import * as queries from '../../graphql/queries'
import { CompleteGroupButtonMutation, Chat, Chats } from '../../graphql/types'
import { useMe } from '../../services/auth.service'

const Style = styled.div`
  position: fixed;
  right: 10px;
  bottom: 10px;

  button {
    min-width: 50px;
    width: 50px;
    height: 50px;
    border-radius: 999px;
    background-color: var(--secondary-bg);
    color: white;
  }
`

const mutation = gql`
  mutation CompleteGroupButtonMutation(
    $userIds: [chat_users_insert_input!]!
    $groupName: String!
    $groupPicture: String
    $ownerId: Int
  ) {
    insert_chat(objects: [{name: $groupName, picture: $groupPicture, owner_id: $ownerId, users:{data: $userIds}}]) {
      returning {
        ...chat
      }
    }
  }
  ${fragments.chat}
`

interface CompleteGroupButtonProps {
  history: History
  users: any
  groupName: string
  groupPicture: string
}

export default ({ history, users, groupName, groupPicture }: CompleteGroupButtonProps) => {
  const me = useMe()

  // business logic required.
  const userIds = users.map(user => { return {user_id: user.id} });
  userIds.push({user_id: me.id});
  const addGroup = useMutation<CompleteGroupButtonMutation.Mutation, CompleteGroupButtonMutation.Variables>(mutation, {
    variables: {
      userIds: userIds,
      groupName,
      groupPicture,
      ownerId: me.id
    },
    update: (client, { data: { insert_chat } }) => {
      client.writeFragment<Chat.Fragment>({
        id: defaultDataIdFromObject(insert_chat.returning[0]),
        fragment: fragments.chat,
        fragmentName: 'chat',
        data: insert_chat.returning[0],
      })

      let chats
      try {
        chats = client.readQuery<Chats.Query>({
          query: queries.chats,
        }).chat
      } catch (e) {}

      if (chats && !chats.some(chat => chat.id === insert_chat.returning[0].id)) {
        chats.unshift(insert_chat.returning[0])

        client.writeQuery<Chats.Query>({
          query: queries.chats,
          data: { chat: chats },
        })
      }

      // now insert group members chat_users
    },
  })

  const onClick = () => {
    addGroup().then(({ data: { insert_chat } }) => {
      history.push(`/chats/${insert_chat.returning[0].id}`)
    })
  }

  return (
    <Style className="CompleteGroupButton">
      <Button variant="contained" color="secondary" onClick={onClick}>
        <ArrowRightIcon />
      </Button>
    </Style>
  )
}
