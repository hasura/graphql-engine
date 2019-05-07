const Express = require('express');
const webpack = require('webpack');

const webpackConfig = require('./dev.config');
const compiler = webpack(webpackConfig);
const appconfig = require('../appconfig');

const host = appconfig.hmrHost;
const port = appconfig.hmrPort;

const serverOptions = {
  contentBase: 'http://' + host + ':' + port,
  logLevel: 'silent',
  hot: true,
  inline: true,
  lazy: false,
  publicPath: webpackConfig.output.publicPath,
  headers: { 'Access-Control-Allow-Origin': '*' },
  stats: { colors: true },
};

const app = new Express();

app.use(require('webpack-dev-middleware')(compiler, serverOptions));
app.use(require('webpack-hot-middleware')(compiler));

app.listen(port, function onAppListening(err) {
  if (err) {
    console.error(err);
  } else {
    console.info(
      '==> 🚧  Webpack development server listening on port %s',
      port
    );
  }
});
