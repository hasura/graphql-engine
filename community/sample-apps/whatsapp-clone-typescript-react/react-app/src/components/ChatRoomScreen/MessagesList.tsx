import gql from 'graphql-tag'
import * as moment from 'moment'
import * as React from 'react'
import { useRef, useEffect } from 'react'
import { useQuery, useMutation } from 'react-apollo-hooks'
import * as ReactDOM from 'react-dom'
import styled from 'styled-components'
import * as fragments from '../../graphql/fragments'
import { useMe } from '../../services/auth.service'
import { MessagesListQuery } from '../../graphql/types'

const Style = styled.div`
  display: block;
  height: calc(100% - 60px);
  width: calc(100% - 30px);
  overflow-y: overlay;
  padding: 0 15px;

  .MessagesList-message {
    display: inline-block;
    position: relative;
    max-width: 100%;
    border-radius: 7px;
    box-shadow: 0 1px 2px rgba(0, 0, 0, 0.15);
    margin-top: 10px;
    margin-bottom: 10px;
    clear: both;

    &::after {
      content: '';
      display: table;
      clear: both;
    }
  }

  .MessagesList-message-mine {
    float: right;
    background-color: #dcf8c6;

    &::before {
      right: -11px;
      background-image: url(/assets/message-mine.png);
    }
  }

  .MessagesList-message-others {
    float: left;
    background-color: #fff;

    &::before {
      left: -11px;
      background-image: url(/assets/message-other.png);
    }
  }

  .MessagesList-message-others::before,
  .MessagesList-message-mine::before {
    content: '';
    position: absolute;
    bottom: 3px;
    width: 12px;
    height: 19px;
    background-position: 50% 50%;
    background-repeat: no-repeat;
    background-size: contain;
  }

  .MessagesList-message-sender {
    font-weight: bold;
    margin-left: 5px;
    margin-top: 5px;
  }

  .MessagesList-message-contents {
    padding: 5px 7px;
    word-wrap: break-word;

    &::after {
      content: ' \00a0\00a0\00a0\00a0\00a0\00a0\00a0\00a0\00a0\00a0\00a0\00a0\00a0\00a0\00a0\00a0\00a0\00a0\00a0';
      display: inline;
    }
  }

  .MessagesList-message-timestamp {
    position: absolute;
    bottom: 2px;
    right: 7px;
    color: gray;
    font-size: 12px;
  }
`

const query = gql`
  query MessagesListQuery($chatId: Int!) {
    chat(where:{id: {_eq: $chatId}}) {
      ...chat
    }
  }
  ${fragments.chat}
`

interface MessagesListProps {
  chatId: string
}

export default ({ chatId }: MessagesListProps) => {
  const me = useMe();
  const {
    data
  } = useQuery<MessagesListQuery.Query, MessagesListQuery.Variables>(query, {
    variables: { chatId: parseInt(chatId,10) },
    suspend: true,
  })
  const messages = data.chat[0].messages;
  const owner_id = data.chat[0].owner_id;
  const selfRef = useRef(null)

  const resetScrollTop = () => {
    if (!selfRef.current) return

    const selfDOMNode = ReactDOM.findDOMNode(selfRef.current) as HTMLElement
    selfDOMNode.scrollTop = Number.MAX_SAFE_INTEGER
  }

  useEffect(resetScrollTop, [selfRef.current])
  useEffect(resetScrollTop, [messages.length])

  return (
    <Style className={name} ref={selfRef}>
      {messages &&
        messages.map(message => (
          <div
            key={message.id+message.created_at}
            className={`MessagesList-message ${
              message.sender.id === me.id ? 'MessagesList-message-mine' : 'MessagesList-message-others'
            }`}
          >
            {owner_id && (
              <div className="MessagesList-message-sender">{message.sender.name}</div>
            )}
            <div className="MessagesList-message-contents">{message.content}</div>
            <span className="MessagesList-message-timestamp">
              {moment(message.created_at).format('HH:mm')}
            </span>
          </div>
        ))}
    </Style>
  )
}
