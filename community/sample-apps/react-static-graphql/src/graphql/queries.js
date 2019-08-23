import gql from "graphql-tag";

const GET_AUTHOR = gql`
  query {
    author {
    	id
    	name
    }
  }
`;

const GET_ARTICLE = gql`
  query($author: Int!) {
    article(where: {author_id: {_eq: $author}}) {
    	id
    	title
    	content
    	created_at
    }
  }
`;

export { GET_ARTICLE, GET_AUTHOR };