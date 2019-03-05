import App from '../components/App'
import Header from '../components/Header'

export default () => (
  <App>
    <Header />
    <article>
      <h1>The Idea Behind This Example</h1>
      <p>
        <a href='https://www.apollographql.com/client/'>Apollo</a> is a GraphQL
        client that allows you to easily query the exact data you need from a
        GraphQL server. In addition to fetching and mutating data, Apollo
        analyzes your queries and their results to construct a client-side cache
        of your data, which is kept up to date as further queries and mutations
        are run, fetching more results from the server.
      </p>
      <p>
        In this simple example, we integrate Apollo seamlessly with{' '}
        <a href='https://github.com/zeit/next.js'>Next</a> by wrapping our pages
        inside a{' '}
        <a href='https://facebook.github.io/react/docs/higher-order-components.html'>
          higher-order component (HOC)
        </a>
        . Using the HOC pattern we're able to pass down a central store of query
        result data created by Apollo into our React component hierarchy defined
        inside each page of our Next application.
      </p>
      <p>
        On initial page load, while on the server and inside getInitialProps, we
        invoke the Apollo method,{' '}
        <a href='https://www.apollographql.com/docs/react/features/server-side-rendering.html#getDataFromTree'>
          getDataFromTree
        </a>
        . This method returns a promise; at the point in which the promise
        resolves, our Apollo Client store is completely initialized.
      </p>
    </article>
  </App>
)
