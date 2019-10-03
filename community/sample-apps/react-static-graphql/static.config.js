import client from './src/apollo'
import {GET_AUTHOR, GET_ARTICLE} from './src/graphql/queries'

export default {
  getSiteData: () => ({
    title: 'React Static with Hasura GraphQL',
  }),
  getRoutes: async () => {
    const {
      data: { author },
    } = await client.query({
      query: GET_AUTHOR,
    })
    return [
      {
        path: '/blog',
        getData: () => ({author}),
        children: author.map(item => ({
          path: `/${item.id.toString()}`,
          component: 'src/containers/Post',
          getData: (resolvedRoute) => {
            const path = resolvedRoute.route.path
            const author_id = path.split("/")[1]
            return client.query({
              query: GET_ARTICLE,
              variables: {author: author_id}
            }).then( (resp) => {
              const articles = resp.data.article;
              if(articles) {
                return {articles}
              } 
              return {articles: []}
            })
          },
        })),
      },
    ]
  },
}
