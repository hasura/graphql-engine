import React from "react"
import PropTypes from "prop-types"

export default class HTML extends React.Component {
  render() {
    return (
      <html {...this.props.htmlAttributes}>
        <head>
          <title>GraphQL Tutorials for Frontend Developers | learn.hasura.io</title>
          <meta charSet="utf-8" />
          <meta httpEquiv="x-ua-compatible" content="ie=edge" />
          <meta
            name="viewport"
            content="width=device-width, initial-scale=1, shrink-to-fit=no"
          />
          <meta name="title" content="GraphQL Tutorials for Frontend Developers | learn.hasura.io" />
          <meta name="description" content="Learn how to integrate GraphQL APIs with your favorite frontend / mobile framework and Hasura GraphQL Engine" />
          <meta property="og:title" content="GraphQL Tutorials for Frontend Developers | learn.hasura.io" />
          <meta property="og:description" content="Learn how to integrate GraphQL APIs with your favorite frontend / mobile framework and Hasura GraphQL Engine" />
          <meta property="og:image" content="https://graphql-engine-cdn.hasura.io/learn-hasura/assets/social-media/twitter-card-homepage.png" />
          <meta property="twitter:card" content="summary_large_image" />
          <meta property="twitter:title" content="GraphQL Tutorials for Frontend Developers | learn.hasura.io" />
          <meta property="twitter:description" content="Learn how to integrate GraphQL APIs with your favorite frontend / mobile framework and Hasura GraphQL Engine" />
          <meta property="twitter:image" content="https://graphql-engine-cdn.hasura.io/learn-hasura/assets/social-media/twitter-card-homepage.png" />
          {this.props.headComponents}
          <link rel="shortcut icon" href="https://graphql-engine-cdn.hasura.io/learn-hasura/assets/homepage/favicon.png" type="image/png" />
          <link rel="stylesheet"
          href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
          integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossOrigin="anonymous" />
          <link rel="stylesheet"
          href="https://use.fontawesome.com/releases/v5.8.2/css/all.css"
          integrity="sha384-oS3vJWv+0UjzBfQzYUhtDYW+Pj2yciDJxpsK1OYPAYjqT085Qq/1cq5FLXAZQ7Ay" crossOrigin="anonymous" />
          <script
          src="https://code.jquery.com/jquery-3.4.1.min.js"
          integrity="sha256-CSXorXvZcTkaix6Yvo6HppcZGetbYMGWSFlBw8HfCJo="
          crossOrigin="anonymous"></script>
          <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
          integrity="sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa"
          crossOrigin="anonymous"/>
        </head>
        <body {...this.props.bodyAttributes}>
          {this.props.preBodyComponents}
          <div
            key={`body`}
            id="___gatsby"
            dangerouslySetInnerHTML={{ __html: this.props.body }}
          />
          {this.props.postBodyComponents}
          <script
          dangerouslySetInnerHTML={{
            __html: `
            $(document).on('click', '.dropdown-menu', function (e) {
              e.stopPropagation();
            });
            $(document).ready(function(){
              $('#myCarousel').carousel({
                interval: 3000,
                cycle: true
              });
            });
            `
          }}
          />
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
