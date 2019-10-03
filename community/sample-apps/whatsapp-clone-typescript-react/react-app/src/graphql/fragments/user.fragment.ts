import gql from 'graphql-tag'

export default gql`
  fragment user on users {
    id
    username
    name
    picture
  }
`
