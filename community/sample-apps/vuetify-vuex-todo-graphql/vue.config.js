const manifestJSON = require('./public/manifest.json')

module.exports = {
  pwa: {
    themeColor: manifestJSON.theme_color,
    workboxOptions: {
      runtimeCaching: [{
        urlPattern: new RegExp('https://fonts.(?:googleapis|gstatic).com/(.*)'),
        handler: 'cacheFirst',
        options: {
          cacheName: 'google-fonts',
          expiration: {
            maxEntries: 30
          },
          cacheableResponse: {
            statuses: [0, 200]
          }
        }
      }]
    }
  },
  chainWebpack: config => {
    config.module
      .rule('graphql')
      .test(/\.gql$/)
      .use('graphql-tag/loader')
      .loader('graphql-tag/loader')
      .end()
  }
}
