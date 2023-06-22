// Copyright IBM Corp. 2017,2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

'use strict'

import { afterAll, beforeAll, expect, test } from '@jest/globals'
import * as openAPIToGraphQL from '../src/index'
import * as Oas3Tools from '../src/oas_3_tools'

import { Volume } from 'memfs'
import FormData from 'form-data'
import { createServer } from 'http'
import fetch from 'cross-fetch'
import { graphql, GraphQLArgs, GraphQLObjectType, GraphQLSchema } from 'graphql'
import processRequest from 'graphql-upload/processRequest'
import { startServer as startAPIServer, stopServer as stopAPIServer } from './file_upload_api_server'

// Set up the schema first
const oas = require('./fixtures/file_upload.json')

const PORT = 3010

// Update PORT for this test case:
oas.servers[0].variables.port.default = String(PORT)

let createdSchema: GraphQLSchema

beforeAll(async () => {
  const [{ schema }] = await Promise.all([
    openAPIToGraphQL.createGraphQLSchema(oas),
    startAPIServer(PORT)
  ])

  createdSchema = schema
})

afterAll(async () => {
  await stopAPIServer()
})

test('All mutation endpoints are found to be present', () => {
  let oasMutCount = 0
  for (let path in oas.paths) {
    for (let method in oas.paths[path]) {
      if (Oas3Tools.isHttpMethod(method) && method !== 'get') oasMutCount++
    }
  }
  const gqlTypes = Object.keys((createdSchema.getTypeMap().Mutation as GraphQLObjectType).getFields()).length
  expect(gqlTypes).toEqual(oasMutCount)
})

test('Registers the graphql-upload Upload scalar type', async () => {
  const query = `{
    __type(name: "Upload") {
      name
      kind
    }
  }`

  const result = await graphql({ schema: createdSchema, source: query })
  expect(result).toEqual({
    data: {
      __type: {
        name: 'Upload',
        kind: 'SCALAR'
      }
    }
  })
})

test('Introspection for mutations returns a mutation matching the custom field specified for the multipart API definition', async () => {
  const query = `{
    __schema {
      mutationType {
        fields {
          name
          args {
            name
            type {
              name
              kind
            }
          }
          type {
            name
            kind
          }
        }
      }
    }
  }`

  const result = await graphql({ schema: createdSchema, source: query })

  expect(result).toEqual({
    data: {
      __schema: {
        mutationType: {
          fields: expect.arrayContaining([
            expect.objectContaining({
              name: 'fileUploadTest',
              args: expect.arrayContaining([
                expect.objectContaining({
                  name: 'uploadInput'
                })
              ])
            })
          ])
        }
      }
    }
  })
})

test('Upload completes without any error', async () => {
  // Setup GraphQL for integration test
  const graphqlServer = createServer(async (req, res) => {
    try {
      const operation = await processRequest(req, res)
      const result = await graphql({ schema: createdSchema, source: operation.query, variableValues: operation.variables as GraphQLArgs['variableValues'] })
      res.end(JSON.stringify(result))
    } catch (e) {
      console.log(e)
    }
  })

  const { port: graphqlServerPort, close: closeGraphQLServer }: any = await new Promise((resolve, reject) => {
    graphqlServer.listen(function (err) {
      if (err) {
        return reject(err)
      }

      return resolve({
        port: this.address().port,
        close: () => this.close()
      })
    })
  })

  const vol = new Volume()

  // Create mocked in memory file for upload
  vol.fromJSON({
    './README.md': '1'
  }, '/app')

  // Prepare request to match GraphQL multipart request spec
  // Reference: https://github.com/jaydenseric/graphql-multipart-request-spec
  const form = new FormData()
  const query = `
    mutation FileUploadTest($file: Upload!) {
      fileUploadTest(uploadInput: { file: $file }) {
        id
        url
      }
    }
  `
  form.append('operations', JSON.stringify({ query, variables: { file: null } }))
  form.append('map', JSON.stringify({ 0: ['variables.file'] }))
  form.append('0', vol.createReadStream('/app/README.md'), {
    filename: 'readme.md',
    filepath: '/app'
  })

  // @ts-ignore
  const uploadResult = await fetch(`http://localhost:${graphqlServerPort}`, { method: 'POST', body: form })
      .then(res => res.json())

  expect(uploadResult.errors).not.toBeDefined()
  expect(uploadResult.data).toBeDefined()
  expect(uploadResult.data.fileUploadTest).toBeDefined()

  closeGraphQLServer()
})
