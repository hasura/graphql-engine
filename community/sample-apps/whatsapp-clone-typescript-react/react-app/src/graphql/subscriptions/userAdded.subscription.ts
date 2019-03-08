import gql from 'graphql-tag'
import * as fragments from '../fragments'

export default gql `
  subscription UserAdded {
    users(order_by:[{id:desc}]) {
      ...user
    }
  }
  ${fragments.user}
`
