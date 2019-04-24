import React from "react"
import PropTypes from "prop-types"

export default class HTML extends React.Component {
  render() {
    return (
      <html {...this.props.htmlAttributes}>
        <head>
          <title>GraphQL Tutorials for frontend developers | learn.hasura.io</title>
          <meta charSet="utf-8" />
          <meta httpEquiv="x-ua-compatible" content="ie=edge" />
          <meta
            name="viewport"
            content="width=device-width, initial-scale=1, shrink-to-fit=no"
          />
          <meta name="title" content="GraphQL Tutorials for frontend developers | learn.hasura.io" />
          <meta name="description" content="Learn how to integrate GraphQL APIs with React, Apollo and Hasura GraphQL Engine" />
          <meta property="og:title" content="GraphQL Tutorials for frontend developers | learn.hasura.io" />
          <meta property="og:description" content="Learn how to integrate GraphQL APIs with React, Apollo and Hasura GraphQL Engine" />
          <meta property="og:image" content="https://storage.googleapis.com/graphql-engine-cdn.hasura.io/learn-hasura/assets/twitter-card.png" />
          <meta property="twitter:card" content="summary_large_image" />
          <meta property="twitter:title" content="GraphQL Tutorials for frontend developers | learn.hasura.io" />
          <meta property="twitter:description" content="Learn how to integrate GraphQL APIs with React, Apollo and Hasura GraphQL Engine" />
          <meta property="twitter:image" content="https://storage.googleapis.com/graphql-engine-cdn.hasura.io/learn-hasura/assets/twitter-card.png" />

          <script async src="https://www.googletagmanager.com/gtag/js?id=UA-59768903-1"></script>
          <script>
            window.dataLayer = window.dataLayer || [];
            function gtag(){dataLayer.push(arguments);}
            gtag('js', new Date());

            gtag('config', 'UA-59768903-1');
          </script>

          {this.props.headComponents}
          <link rel="shortcut icon" href="./images/favicon.png" type="image/png" />
          <link rel="stylesheet"
          href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
          integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossOrigin="anonymous" />
          <link rel="stylesheet"
          href="https://use.fontawesome.com/releases/v5.3.1/css/all.css"
          integrity="sha384-mzrmE5qonljUremFsqc01SB46JvROS7bZs3IO2EmfFsd15uHvIt+Y8vEf7N7fWAU" crossOrigin="anonymous" />
        </head>
        <body {...this.props.bodyAttributes}>
          {this.props.preBodyComponents}
          <div
            key={`body`}
            id="___gatsby"
            dangerouslySetInnerHTML={{ __html: this.props.body }}
          />
          {this.props.postBodyComponents}
        </body>
      </html>
    )
  }
}

HTML.propTypes = {
  htmlAttributes: PropTypes.object,
  headComponents: PropTypes.array,
  bodyAttributes: PropTypes.object,
  preBodyComponents: PropTypes.array,
  body: PropTypes.string,
  postBodyComponents: PropTypes.array,
}
