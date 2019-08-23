import React from 'react'
import { Root, Routes } from 'react-static'
import { Link } from '@reach/router'

import { ApolloProvider } from 'react-apollo'
import client from './apollo'

import './app.css'

function App() {
  return (
    <ApolloProvider client={client}>
      <Root>
        <nav>
          <Link to="/">Home</Link>
          <Link to="/about">About</Link>
          <Link to="/blog">Blog</Link>
        </nav>
        <div className="content">
          <Routes />
        </div>
      </Root>
    </ApolloProvider>
  )
}

export default App
