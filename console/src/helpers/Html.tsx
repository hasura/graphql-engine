import React from 'react';
import { env } from './localDev';
/**
 * Wrapper component containing HTML metadata and boilerplate tags.
 * Used in server-side code only to wrap the string output of the
 * rendered route component.
 *
 * The only thing this component doesn't (and can't) include is the
 * HTML doctype declaration, which is added to the rendered output
 * by the server.js file.
 */

type Assets = Record<'styles' | 'javascript', Record<string, string>>;

type HtmlProps = {
  assets: Assets;
  component: React.ReactNode;
  baseDomain: string;
};

const Html: React.FC<HtmlProps> = props => {
  const { assets } = props;
  return (
    <html lang="en-US">
      <head>
        <link rel="icon" type="image/png" href="/rstatic/favicon_green.png" />
        {Object.keys(assets.styles).map((style, key) => (
          <link
            href={assets.styles[style]}
            key={key}
            media="screen, projection"
            rel="stylesheet"
            type="text/css"
          />
        ))}

        <script
          dangerouslySetInnerHTML={{
            __html: env,
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
        <script src={`${assets.javascript.main}`} charSet="UTF-8" />
      </body>
    </html>
  );
};

export default Html;
