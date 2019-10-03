import * as React from 'react'
import { BrowserRouter, Route, Redirect } from 'react-router-dom'
import ChatRoomScreen from './components/ChatRoomScreen'
import NewChatScreen from './components/NewChatScreen'
import AnimatedSwitch from './components/AnimatedSwitch'
import AuthScreen from './components/AuthScreen'
import ChatsListScreen from './components/ChatsListScreen'
import GroupDetailsScreen from './components/GroupDetailsScreen'
import SettingsScreen from './components/SettingsScreen'
import NewGroupScreen from './components/NewGroupScreen'
import { withAuth } from './services/auth.service'

const RedirectToChats = () => (
  <Redirect to="/chats" />
)

export default () => (
  <BrowserRouter>
    <AnimatedSwitch>
      <Route exact path="/sign-(in|up)" component={AuthScreen} />
      <Route exact path="/chats" component={withAuth(ChatsListScreen)} />
      <Route exact path="/settings" component={withAuth(SettingsScreen)} />
      <Route exact path="/chats/:chatId" component={withAuth(ChatRoomScreen)} />
      <Route exact path="/new-chat" component={withAuth(NewChatScreen)} />
      <Route exact path="/new-chat/group" component={withAuth(NewGroupScreen)} />
      <Route exact path="/new-chat/group/details" component={withAuth(GroupDetailsScreen)} />
      <Route exact path="/chats/:chatId/details" component={withAuth(GroupDetailsScreen)} />
      <Route component={RedirectToChats} />
    </AnimatedSwitch>
  </BrowserRouter>
)
