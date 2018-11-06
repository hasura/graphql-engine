import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Helmet from 'react-helmet';
/**
 * Wrapper component containing HTML metadata and boilerplate tags.
 * Used in server-side code only to wrap the string output of the
 * rendered route component.
 *
 * The only thing this component doesn't (and can't) include is the
 * HTML doctype declaration, which is added to the rendered output
 * by the server.js file.
 */
export default class Html extends Component {
  static propTypes = {
    assets: PropTypes.object,
    component: PropTypes.node,
    baseDomain: PropTypes.string,
  };

  render() {
    const { assets } = this.props;
    const head = Helmet.rewind();

    return (
      <html lang="en-us">
        <head>
          <link rel="icon" type="image/png" href="/rstatic/favicon.png" />
          <title>GraphiQL Online with Headers | Built by Hasura</title>
          <meta name="title" content="GraphiQL Online with Headers | Built by Hasura" />
          <meta name="description" content="An online version of GraphiQL. Manage headers easily. Test your GraphQL servers" />

          {Object.keys(assets.styles).map((style, key) => (
            <link
              href={assets.styles[style]}
              key={key}
              media="screen, projection"
              rel="stylesheet"
              type="text/css"
              charSet="UTF-8"
            />
          ))}
          <link
            rel="stylesheet"
            href="//maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
            integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u"
            crossOrigin="anonymous"
          />

          <script
            dangerouslySetInnerHTML={{
              __html: `window.__env={
                graphqlEndpoint: '${process.env.GRAPHQL_ENDPOINT}',
                query: '${process.env.QUERY_STRING}',
                headers: '${process.env.HEADER_STRING}',
                variables: '${process.env.VARIABLE_STRING}'
              };`,
            }}
          />
        </head>
        <body>
          <style
            dangerouslySetInnerHTML={{
              __html: `
            .content {
              display: 'none';
              opacity: 0;
              transition: opacity .20s linear;
            }
            .content.show {
              display: 'block';
              opacity: 1;
              transition: opacity .20s linear;
            }
          `,
            }}
          />
          <div
            id="loading"
            dangerouslySetInnerHTML={{
              __html: `<div class="page-loading" style="
                min-height: 100vh;
                width: 100%;
                display: flex;
                align-items: center;
                font-family: sans-serif;
                justify-content: center;
            ">
            <span class="" style="
                font-size: 2em;
                margin-top: -3em;
                color: #848484;
            ">
              Loading...
              </span>
            </div>`,
            }}
          />

          <div id="content" className="content" />
          <script src={assets.javascript.main} charSet="UTF-8" />
        </body>
      </html>
    );
  }
}
