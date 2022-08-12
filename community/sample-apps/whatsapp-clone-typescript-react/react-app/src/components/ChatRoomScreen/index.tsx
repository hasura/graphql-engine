import * as React from 'react'
import { Suspense } from 'react'
import { RouteComponentProps } from 'react-router-dom'
import styled from 'styled-components'
import Navbar from '../Navbar'
import ChatNavbar from './ChatNavbar'
import MessageBox from './MessageBox'
import MessagesList from './MessagesList'

const Style = styled.div`
  .ChatScreen-body {
    position: relative;
    background: url(/assets/chat-background.jpg);
    width: 100%;
    height: calc(100% - 56px);

    .MessagesList {
      position: absolute;
      height: calc(100% - 60px);
      top: 0;
    }

    .MessageBox {
      position: absolute;
      bottom: 0;
      left: 0;
    }

    .AddChatButton {
      right: 0;
      bottom: 0;
    }
  }
`

export default ({ match, history }: RouteComponentProps) => {
  const chatId = match.params.chatId

  return (
    <Style className="ChatScreen Screen">
      <Navbar>
        <Suspense fallback={null}>
          <ChatNavbar chatId={chatId} history={history} />
        </Suspense>
      </Navbar>
      <div className="ChatScreen-body">
        <Suspense fallback={null}>
          <MessagesList chatId={chatId} />
        </Suspense>
        <Suspense fallback={null}>
          <MessageBox chatId={chatId} />
        </Suspense>
      </div>
    </Style>
  )
}
