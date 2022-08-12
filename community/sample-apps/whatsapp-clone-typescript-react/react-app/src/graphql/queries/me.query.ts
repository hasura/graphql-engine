import gql from 'graphql-tag'
import * as fragments from '../fragments'

export default gql `
  query Me {
    users {
      ...user
    }
  }
  ${fragments.user}
`
