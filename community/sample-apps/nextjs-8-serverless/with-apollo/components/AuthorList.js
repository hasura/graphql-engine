import { Query } from 'react-apollo'
import gql from 'graphql-tag'

export const authorQuery = gql`
  query author($skip: Int!) {
    author(offset: $skip, limit: 5) {
      id
      name
    }
    author_aggregate {
      aggregate {
        count
      }
    }  
}
`
export const authorQueryVars = {
  skip: 0,
}

export default function AuthorList () {
  return (
    <Query query={authorQuery} variables={authorQueryVars}>
      {({ loading, error, data: { author, author_aggregate }, fetchMore }) => {
        if (error) return <ErrorMessage message='Error loading authors.' />
        if (loading) return <div>Loading</div>

        const areMoreAuthors = author.length < author_aggregate.aggregate.count
        return (
          <section>
            <ul>
              {author.map((a, index) => (
                <li key={a.id}>
                  <div>
                    <span>{index + 1}. </span>
                    <a>{a.name}</a>
                  </div>
                </li>
              ))}
            </ul>
            {areMoreAuthors ? (
              <button onClick={() => loadMoreAuthors(author, fetchMore)}>
                {' '}
                {loading ? 'Loading...' : 'Show More'}{' '}
              </button>
            ) : (
              ''
            )}
            <style jsx>{`
              section {
                padding-bottom: 20px;
              }
              li {
                display: block;
                margin-bottom: 10px;
              }
              div {
                align-items: center;
                display: flex;
              }
              a {
                font-size: 14px;
                margin-right: 10px;
                text-decoration: none;
                padding-bottom: 0;
                border: 0;
              }
              span {
                font-size: 14px;
                margin-right: 5px;
              }
              ul {
                margin: 0;
                padding: 0;
              }
              button:before {
                align-self: center;
                border-style: solid;
                border-width: 6px 4px 0 4px;
                border-color: #ffffff transparent transparent transparent;
                content: '';
                height: 0;
                margin-right: 5px;
                width: 0;
              }
            `}</style>
          </section>
        )
      }}
    </Query>
  )
}

function loadMoreAuthors (author, fetchMore) {
  fetchMore({
    variables: {
      skip: author.length
    },
    updateQuery: (previousResult, { fetchMoreResult }) => {
      if (!fetchMoreResult) {
        return previousResult
      }
      return Object.assign({}, previousResult, {
        // Append the new results to the old one
        author: [...previousResult.author, ...fetchMoreResult.author]
      })
    }
  })
}
