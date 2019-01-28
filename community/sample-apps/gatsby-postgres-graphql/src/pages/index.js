import React from "react"
import AuthorList from "../components/AuthorList"

const Index = ({ data }) => (
  <div>
    <h1>My Authors </h1>
    <AuthorList authors={data.hasura.author} />
  </div>
)

export default Index;

export const query = graphql`
  query AuthorQuery {
    hasura {
      author {
        id
        name
      }
    }
  }
`
