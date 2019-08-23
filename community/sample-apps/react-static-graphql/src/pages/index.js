import React from 'react'
import { withSiteData } from 'react-static'

export default withSiteData(() => (
  <div style={{ textAlign: 'center' }}>
    <h1>Welcome to React-Static</h1>
    <p>This demo integrates <a href="https://github.com/hasura/graphql-engine">Hasura GraphQL Engine</a> and allows you to generate static sites with your Postgres database using GraphQL</p>
  </div>
))
