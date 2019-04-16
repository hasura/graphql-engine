import Button from '@material-ui/core/Button'
import SendIcon from '@material-ui/icons/Send'
import gql from 'graphql-tag'
import * as React from 'react'
import { useState } from 'react'
import { useMutation } from 'react-apollo-hooks'
import styled from 'styled-components'
import { time as uniqid } from 'uniqid'
import * as fragments from '../../graphql/fragments'
import { MessageBoxMutation, MessagesListQueryLocal } from '../../graphql/types'
import { useMe } from '../../services/auth.service'

const Style = styled.div`
  display: flex;
  height: 50px;
  padding: 5px;
  width: calc(100% - 10px);

  .MessageBox-input {
    width: calc(100% - 50px);
    border: none;
    border-radius: 999px;
    padding: 10px;
    padding-left: 20px;
    padding-right: 20px;
    font-size: 15px;
    outline: none;
    box-shadow: 0 1px silver;
    font-size: 18px;
    line-height: 45px;
  }

  .MessageBox-button {
    min-width: 50px;
    width: 50px;
    border-radius: 999px;
    background-color: var(--primary-bg);
    margin: 0 5px;
    margin-right: 0;
    color: white;
    padding-left: 20px;
    svg {
      margin-left: -3px;
    }
  }
`

const mutation = gql`
  mutation MessageBoxMutation($chatId: Int!, $content: String!, $sender_id: Int!) {
    insert_message(objects: [{chat_id: $chatId, content: $content, sender_id: $sender_id}]) {
      affected_rows
      returning {
        id
        content
        created_at
        sender_id
        chat_id
        chat {
          id
        }
        sender {
          id
          name
        }
        recipients {
          user {
            id
          }
          message {
            id
            chat {
              id
            }
          }
          received_at
          read_at
        }
      }
    }
  }
`
interface MessageBoxProps {
  chatId: string
}

export default ({ chatId }: MessageBoxProps) => {
  const [message, setMessage] = useState('')
  const me = useMe()
  const senderId = me.id;

  const addMessage = useMutation<MessageBoxMutation.Mutation, MessageBoxMutation.Variables>(
    mutation,
    {
      variables: {
        chatId: parseInt(chatId,10),
        content: message,
        sender_id: senderId
      },
      update: (client, { data: { insert_message } }) => {
        const chatQuery = gql`
          query MessagesListQueryLocal($chatId: Int!) {
            chat(where:{id: {_eq: $chatId}}) {
              ...chat
            }
          }
          ${fragments.chat}
        `

        let chatData
        try {
          chatData = client.readQuery<MessagesListQueryLocal.Query, MessagesListQueryLocal.Variables>(
            {query: chatQuery, variables: {chatId: parseInt(chatId,10)}})
        } catch(e) {
          console.error(e);
        }
        const finalChatData = {...chatData};
        finalChatData.chat[0].messages.push(insert_message.returning[0]);
        try {
          client.writeQuery<MessagesListQueryLocal.Query, MessagesListQueryLocal.Variables>(
            {query: chatQuery, variables: {chatId: parseInt(chatId,10)}, data: finalChatData})
        } catch(e) {
          console.error(e);
        }
      },
    },
  )

  const onKeyPress = e => {
    if (e.charCode === 13) {
      submitMessage()
    }
  }

  const onChange = ({ target }) => {
    setMessage(target.value)
  }

  const submitMessage = () => {
    if (!message) return

    addMessage()
    setMessage('')
  }

  return (
    <Style className="MessageBox">
      <input
        className="MessageBox-input"
        type="text"
        placeholder="Type a message"
        value={message}
        onKeyPress={onKeyPress}
        onChange={onChange}
      />
      <Button
        variant="contained"
        color="primary"
        className="MessageBox-button"
        onClick={submitMessage}
      >
        <SendIcon />
      </Button>
    </Style>
  )
}
