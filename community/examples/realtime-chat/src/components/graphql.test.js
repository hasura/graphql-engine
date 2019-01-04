'use strict'

const EasyGraphQLTester = require('easygraphql-tester')
const fs = require('fs')
const path = require('path')

import { subscribeToNewMessages, emitOnlineEvent } from './chat'
import { addUser } from './LandingPage'
import { fetchOnlineUsersSubscription } from './OnlineUsers'
import { fetchMessages } from './RenderMessages'
import { insertMessage, emitTypingEvent } from './Textbox'
import { getUserTyping } from './TypingIndicator'

const userSchema = fs.readFileSync(path.join(__dirname, '..', '..', 'schema.graphql'), 'utf8')

const tester = new EasyGraphQLTester(userSchema)

it('Should subscribeToNewMessages', () => {
  tester.test(true, subscribeToNewMessages)
});

it('Should emitOnlineEvent', () => {
  tester.test(true, emitOnlineEvent)
});

it('Should addUser', () => {
  tester.test(true, addUser)
});

it('Should fetchOnlineUsersSubscription', () => {
  tester.test(true, fetchOnlineUsersSubscription)
});

it('Should fetchMessages', () => {
  tester.test(true, fetchMessages)
});

it('Should insertMessage', () => {
  tester.test(true, insertMessage)
});

it('Should emitTypingEvent', () => {
  tester.test(true, emitTypingEvent)
});

it('Should getUserTyping', () => {
  tester.test(true, getUserTyping)
});