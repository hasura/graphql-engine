import React from 'react'
import Layout from '../components/layout'

const Home = props => (
  <Layout>
    <h1>Authentication example with JWT and Hasura GraphQL</h1>

    <p>Steps to test the functionality:</p>

    <ol>
      <li>Click signup and enter username and password.</li>
      <li>
        Click home and click articles again, notice how your session is being
        used through a token.
      </li>
      <li>
        Click logout and try to go to articles again. You'll get redirected to
        the `/login` route.
      </li>
    </ol>
    <style jsx>{`
      li {
        margin-bottom: 0.5rem;
      }
    `}</style>
  </Layout>
)

export default Home
