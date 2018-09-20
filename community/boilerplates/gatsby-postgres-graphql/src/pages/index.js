import React from "react"
import AuthorList from "../components/AuthorList"

const Index = ({ data }) => (
  <div>
    <h1>My Authors </h1>
    <AuthorList authors={data.hasuraGraphQl.author} />
  </div>
)

export default Index;

export const query = graphql`
  query AuthorQuery {
    hasuraGraphQl {
      author {
        id
        name
      }
    }
  }
`
