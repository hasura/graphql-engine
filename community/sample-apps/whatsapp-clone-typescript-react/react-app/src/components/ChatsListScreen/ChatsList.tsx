import List from '@material-ui/core/List'
import ListItem from '@material-ui/core/ListItem'
import gql from 'graphql-tag'
import { History } from 'history'
import * as moment from 'moment'
import * as React from 'react'
import { useQuery } from 'react-apollo-hooks'
import * as ReactDOM from 'react-dom'
import styled from 'styled-components'
import * as fragments from '../../graphql/fragments'
import { ChatsListQuery } from '../../graphql/types'
import { useMe } from '../../services/auth.service';

const Style = styled.div`
  height: calc(100% - 56px);
  overflow-y: overlay;

  .ChatsList-chats-list {
    padding: 0;
  }

  .ChatsList-chat-item {
    height: 76px;
    padding: 0 15px;
    display: flex;
  }

  .ChatsList-profile-pic {
    height: 50px;
    width: 50px;
    object-fit: cover;
    border-radius: 50%;
  }

  .ChatsList-info {
    width: calc(100% - 60px);
    height: calc(100% - 30px);
    padding: 15px 0;
    margin-left: 10px;
    border-bottom: 0.5px solid silver;
    position: relative;
  }

  .ChatsList-name {
    margin-top: 5px;
  }

  .ChatsList-last-message {
    color: gray;
    font-size: 15px;
    margin-top: 5px;
    text-overflow: ellipsis;
    overflow: hidden;
    white-space: nowrap;
  }

  .ChatsList-timestamp {
    position: absolute;
    color: gray;
    top: 20px;
    right: 0;
    font-size: 13px;
  }
`

// user_id not equal to currently logged in user
const query = gql`
  query ChatsListQuery($userId: Int!) {
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

interface ChatsListProps {
  history: History
}

export default ({ history }: ChatsListProps) => {
  const me = useMe();
  const {
    data: { chat },
  } = useQuery<ChatsListQuery.Query, ChatsListQuery.Variables>(query, { variables: {userId: me.id}, suspend: true })

  const navToChat = chatId => {
    history.push(`chats/${chatId}`)
  }
  return (
    <Style className="ChatsList">
      <List className="ChatsList-chats-list">
        {chat.map(chat => {
          return (
            <ListItem
              key={chat.id}
              className="ChatsList-chat-item"
              button
              onClick={navToChat.bind(null, chat.id)}
            >
              <img
                className="ChatsList-profile-pic"
                src={
                  chat.users[0].user.picture ||
                  (chat.owner_id
                    ? '/assets/default-group-pic.jpg'
                    : '/assets/default-profile-pic.jpg')
                }
              />
              <div className="ChatsList-info">
                <div className="ChatsList-name">{chat.owner_id ? chat.name : chat.users[0].user.username}</div>
                {chat.messages && chat.messages[chat.messages.length-1] && (
                  <React.Fragment>
                    <div className="ChatsList-last-message">{chat.messages[chat.messages.length-1].content}</div>
                    <div className="ChatsList-timestamp">
                      {moment(chat.messages[chat.messages.length-1].created_at).format('HH:mm')}
                    </div>
                  </React.Fragment>
                )}
              </div>
            </ListItem>
          );
        })}
      </List>
    </Style>
  )
}
