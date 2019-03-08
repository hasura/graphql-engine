import gql from 'graphql-tag'
import * as fragments from '../fragments'

export default gql `
  subscription message {
    message(order_by:[{id: desc}]) {
      ...message
    }
  }
  ${fragments.message}
`
