import { withData } from 'next-apollo'
import { HttpLink } from 'apollo-link-http'

const config = {
  link: new HttpLink({
    uri: 'http://localhost:8080/v1/graphql', // <- Configure GraphQL Server URL (must be absolute)
  })
}

export default withData(config)
