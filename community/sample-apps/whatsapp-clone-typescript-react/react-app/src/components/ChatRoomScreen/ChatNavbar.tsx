import Button from '@material-ui/core/Button'
import List from '@material-ui/core/List'
import ListItem from '@material-ui/core/ListItem'
import Popover from '@material-ui/core/Popover'
import ArrowBackIcon from '@material-ui/icons/ArrowBack'
import DeleteIcon from '@material-ui/icons/Delete'
import InfoIcon from '@material-ui/icons/Info'
import MoreIcon from '@material-ui/icons/MoreVert'
import gql from 'graphql-tag'
import { History } from 'history'
import * as React from 'react'
import { useState } from 'react'
import { useQuery, useMutation } from 'react-apollo-hooks'
import styled from 'styled-components'
import * as fragments from '../../graphql/fragments'
import * as queries from '../../graphql/queries'
import { useMe } from '../../services/auth.service';
import { ChatList, DeleteChat, ChatsListCacheQuery } from '../../graphql/types'

const Style = styled.div`
  padding: 0;
  display: flex;
  flex-direction: row;

  margin-left: -20px;
  .ChatNavbar-title {
    line-height: 56px;
  }

  .ChatNavbar-back-button {
    color: var(--primary-text);
  }

  .ChatNavbar-picture {
    height: 40px;
    width: 40px;
    margin-top: 3px;
    margin-left: -22px;
    object-fit: cover;
    padding: 5px;
    border-radius: 50%;
  }

  .ChatNavbar-rest {
    flex: 1;
    justify-content: flex-end;
  }

  .ChatNavbar-options-btn {
    float: right;
    height: 100%;
    font-size: 1.2em;
    margin-right: -15px;
    color: var(--primary-text);
  }

  .ChatNavbar-options-item svg {
    margin-right: 10px;
    padding-left: 15px;
  }
`

const query = gql`
  query ChatList($chatId: Int!, $userId: Int!) {
    chat_users(where:{chat_id: {_eq: $chatId}, user_id: {_neq: $userId}}) {
      chat {
        ...chat
      }
      user {
        ...user
      }
    }
  }
  ${fragments.chat}
  ${fragments.user}
`

const queryCache = gql`
  query ChatsListCacheQuery($userId: Int!) {
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

const mutation = gql`
  mutation deleteChat($chatId: Int!) {
    delete_chat_users(where:{chat_id:{_eq: $chatId}}) {
      affected_rows
    }
    delete_message(where:{chat_id:{_eq: $chatId}}) {
      affected_rows
    }
    delete_chat(where:{id: {_eq: $chatId}}) {
      affected_rows
    }
  }
`

interface ChatNavbarProps {
  chatId: string
  history: History
}

export default ({ chatId, history }: ChatNavbarProps) => {
  const me = useMe();
  const parsedChatId = parseInt(chatId,10)
  const {
    data: { chat_users },
  } = useQuery<ChatList.Query, ChatList.Variables>(query, {
    variables: { chatId: parsedChatId, userId: me.id },
    suspend: true,
  })
  const removeChat = useMutation<DeleteChat.Mutation, DeleteChat.Variables>(
    mutation,
    {
      variables: { chatId: parsedChatId },
      update: (client, { data: { delete_chat } }) => {
        let chats
        try {
          chats = client.readQuery<ChatsListCacheQuery.Query, ChatsListCacheQuery.Variables>({
            query: queryCache,
            variables: {userId: me.id}
          }).chat
        } catch(e) {
          console.error(e)
        }
        if (chats) {
          // filter current parsedChatId
          chats = chats.filter((chat) => chat.id !== parsedChatId);
          try {
            client.writeQuery<ChatsListCacheQuery.Query, ChatsListCacheQuery.Variables>({
              query: queryCache,
              variables: {userId: me.id},
              data: { chat: chats },
            })
          } catch(e) {
            console.error(e)
          }
        }
      },
    },
  )
  const [popped, setPopped] = useState(false)

  const navToChats = () => {
    history.push('/chats')
  }

  const navToGroupDetails = () => {
    setPopped(false)
    history.push(`/chats/${chatId}/details`, { chat_users })
  }

  const handleRemoveChat = () => {
    setPopped(false)
    removeChat().then(navToChats)
  }
  let picture = chat_users[0].chat.owner_id ? chat_users[0].chat.picture : chat_users[0].user.picture;
  if(!picture) {
    picture = chat_users[0].chat.owner_id ? '/assets/default-group-pic.jpg' : '/assets/default-profile-pic.jpg';
  }
  return (
    <Style className={name}>
      <Button className="ChatNavbar-back-button" onClick={navToChats}>
        <ArrowBackIcon />
      </Button>
      <img
        className="ChatNavbar-picture"
        src={picture}
      />
      <div className="ChatNavbar-title">{chat_users[0].chat.owner_id ? chat_users[0].chat.name : chat_users[0].user.username}</div>
      <div className="ChatNavbar-rest">
        <Button className="ChatNavbar-options-btn" onClick={setPopped.bind(null, true)}>
          <MoreIcon />
        </Button>
      </div>
      <Popover
        open={popped}
        onClose={setPopped.bind(null, false)}
        anchorOrigin={{
          vertical: 'top',
          horizontal: 'right',
        }}
        transformOrigin={{
          vertical: 'top',
          horizontal: 'right',
        }}
      >
        <Style style={{ marginLeft: '-15px' }}>
          <List>
            {chat_users[0].chat.owner_id && (
              <ListItem className="ChatNavbar-options-item" button onClick={navToGroupDetails}>
                <InfoIcon />
                Details
              </ListItem>
            )}
            <ListItem className="ChatNavbar-options-item" button onClick={handleRemoveChat}>
              <DeleteIcon />
              Delete
            </ListItem>
          </List>
        </Style>
      </Popover>
    </Style>
  )
}
