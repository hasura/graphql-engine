'use strict'
const merge = require('webpack-merge')
const prodEnv = require('./prod.env')

module.exports = merge(prodEnv, {
  NODE_ENV: '"development"',
  GRAPHQL_ENDPOINT: '"http://localhost:8080/v1alpha1/graphql"'
})
