import React from "react"
import PropTypes from "prop-types"

export default class HTML extends React.Component {
  render() {
    return (
      <html {...this.props.htmlAttributes}>
        <head>
          <title>GraphQL tutorials</title>
          <meta charSet="utf-8" />
          <meta httpEquiv="x-ua-compatible" content="ie=edge" />
          <meta
            name="viewport"
            content="width=device-width, initial-scale=1, shrink-to-fit=no"
          />
          <meta title="GraphQL tutorials" />
          <meta name="title" content="GraphQL tutorials" />
          <meta name="description" content="GraphQL tutorials" />
          <meta property="og:title" content="GraphQL tutorials" />
          <meta property="og:description" content="GraphQL tutorials" />
          <meta property="og:image" content="https://adoring-carson-d8a8e1.netlify.com/card.png" />
          <meta property="twitter:card" content="summary_large_image" />
          <meta property="twitter:title" content="GraphQL tutorials" />
          <meta property="twitter:description" content="GraphQL tutorials" />
          <meta property="twitter:image" content="https://adoring-carson-d8a8e1.netlify.com/card.png" />

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
