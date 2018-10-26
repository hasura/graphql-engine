import Express from 'express';
import React from 'react';
import ReactDOM from 'react-dom/server';
import config from './config';
// import favicon from 'serve-favicon';
import path from 'path';
import Html from './helpers/Html';
import http from 'http';

global.__DISABLE_SSR__ = true;

// Express middleware
const app = new Express();
const server = new http.Server(app);

/* Constants */
/* Routes and middleware */
// app.use(favicon(path.join(__dirname, '..', 'static', 'favicon.ico')));
app.use('/rstatic', Express.static(path.join(__dirname, '..', 'static')));

app.use((req, res) => {
  if (__DEVELOPMENT__) {
    // Do not cache webpack stats: the script file would change since
    // hot module replacement is enabled in the development env
    webpackIsomorphicTools.refresh();
  }

  // Initialize the store here
  function hydrateOnClient() {
    res.send(
      '<!doctype html>\n' +
        ReactDOM.renderToString(
          <Html assets={webpackIsomorphicTools.assets()} />
        )
    );
  }

  if (__DISABLE_SSR__) {
    hydrateOnClient();
    return;
  }
});

if (config.port) {
  server.listen(config.port, config.host, err => {
    if (err) {
      console.error(err);
    }
    console.info(
      '==> 💻  Open http://%s:%s in a browser to view the app.',
      config.host,
      config.port
    );
  });
} else {
  console.error(
    '==>     ERROR: No PORT environment variable has been specified'
  );
}
