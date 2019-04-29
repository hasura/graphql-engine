import React from "react"
import PropTypes from "prop-types"

export default class HTML extends React.Component {
  render() {
    return (
      <html {...this.props.htmlAttributes}>
        <head>
          <meta charSet="utf-8" />
          <meta httpEquiv="x-ua-compatible" content="ie=edge" />
          <meta
            name="viewport"
            content="width=device-width, initial-scale=1, shrink-to-fit=no"
          />
          <meta name="title" content="A GraphQL course for Vue developers | Hasura" />
          <meta name="description" content="A concise and powerful tutorial that covers fundamental concepts of both GraphQL and using GraphQL in Vue" />
          <meta property="og:title" content="A GraphQL course for Vue developers | Hasura" />
          <meta property="og:description" content="A concise and powerful tutorial that covers fundamental concepts of both GraphQL and using GraphQL in Vue" />
          <meta property="og:image" content="https://storage.googleapis.com/graphql-engine-cdn.hasura.io/learn-hasura/assets/twitter-card.png" />
          <meta property="twitter:card" content="summary_large_image" />
          <meta property="twitter:title" content="A GraphQL course for Vue developers | Hasura" />
          <meta property="twitter:description" content="A concise and powerful tutorial that covers fundamental concepts of both GraphQL and using GraphQL in Vue" />
          <meta property="twitter:image" content="https://storage.googleapis.com/graphql-engine-cdn.hasura.io/learn-hasura/assets/twitter-card.png" />
          <link rel="stylesheet"
          href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
          integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossOrigin="anonymous" />
          <script
  			  src="https://code.jquery.com/jquery-3.3.1.min.js"
  			  integrity="sha256-FgpCb/KJQlLNfOu91ta32o/NMZxltwRo8QtmkMRdAu8="
  			  crossOrigin="anonymous"></script>
          <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js" integrity="sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa" crossOrigin="anonymous"></script>
          {this.props.headComponents}
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
