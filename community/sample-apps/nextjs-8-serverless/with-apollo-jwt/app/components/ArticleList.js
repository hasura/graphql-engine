import { Query } from 'react-apollo'
import gql from 'graphql-tag'

export const articleQuery = gql`
  query {
    article {
      id
      title
    }
}
`
export default function ArticleList () {
  return (
    <Query query={articleQuery}>
      {({ loading, error, data: { article} }) => {
        if (error) return <ErrorMessage message='Error loading articles.' />
        if (loading) return <div>Loading</div>

        return (
          <section>
            <ul>
              {article.map((a, index) => (
                <li key={a.id}>
                  <div>
                    <span>{index + 1}. </span>
                    <a>{a.title}</a>
                  </div>
                </li>
              ))}
            </ul>
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

