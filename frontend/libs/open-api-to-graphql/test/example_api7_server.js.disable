// Copyright IBM Corp. 2017,2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

'use strict'

const express = require('express')
const aedes = require('aedes')
const net = require('net')
const bodyParser = require('body-parser')

const app = express()

let httpServer
let mqttBroker
let tcpServer

const Devices = {
  'Audio-player': {
    name: 'Audio-player',
    userName: 'johnny'
  },
  Drone: {
    name: 'Drone',
    userName: 'eric'
  }
}

/**
 * Starts the server at the given port
 */
function startServers(HTTP_PORT, MQTT_PORT) {
  mqttBroker = aedes({
    published: (packet, client, cb) => {
      if (packet.topic.startsWith('$SYS')) {
        return cb()
      }
      console.log(
        `MQTT packet published on ${packet.topic} by ${
          client ? client.id : 'broker'
        }`
      )
      cb()
    }
  })

  tcpServer = net.createServer(mqttBroker.handle)

  app.use(bodyParser.json())

  app.get('/api/user', (req, res) => {
    res.send({
      name: 'Arlene L McMahon'
    })
  })

  app.get('/api/devices', (req, res) => {
    res.status(200).send(Object.values(Devices))
  })

  app.post('/api/devices', (req, res) => {
    if (req.body.userName && req.body.name) {
      const device = req.body
      Devices[device.name] = device
      const packet = {
        topic: `/api/${device.userName}/devices/${req.method.toUpperCase()}/${
          device.name
        }`,
        payload: Buffer.from(JSON.stringify(device))
      }
      mqttBroker.publish(packet)
      res.status(200).send(device)
    } else {
      res.status(404).send({
        message: 'Wrong device schema'
      })
    }
  })

  app.get('/api/devices/:deviceName', (req, res) => {
    if (req.params.deviceName in Devices) {
      res.status(200).send(Devices[req.params.deviceName])
    } else {
      res.status(404).send({
        message: 'Wrong device ID.'
      })
    }
  })

  app.put('/api/devices/:deviceName', (req, res) => {
    if (req.params.deviceName in Devices) {
      if (req.body.userName && req.body.name) {
        const device = req.body
        delete Devices[req.params.deviceName]
        Devices[device.deviceName] = device
        const packet = {
          topic: `/api/${device.userName}/devices/${req.method.toUpperCase()}/${
            device.name
          }`,
          payload: Buffer.from(JSON.stringify(device))
        }
        mqttBroker.publish(packet)
        res.status(200).send(device)
      } else {
        res.status(404).send({
          message: 'Wrong device schema'
        })
      }
    } else {
      res.status(404).send({
        message: 'Wrong device ID.'
      })
    }
  })

  // mqttBroker.on('client', client => {
  //   console.log(`MQTT client connected`, client ? client.id : client)
  // })

  // mqttBroker.on('subscribe', (subscriptions, client) => {
  //   console.log(
  //     `MQTT client ${
  //       client ? client.id : client
  //     } subscribed to topic(s) ${subscriptions.map(s => s.topic).join('\n')}`
  //   )
  // })

  // mqttBroker.on('unsubscribe', (subscriptions, client) => {
  //   console.log(
  //     `MQTT client ${
  //       client ? client.id : client
  //     } unsubscribed from topic(s) ${subscriptions.join('\n')}`
  //   )
  // })

  return Promise.all([
    (httpServer = app.listen(HTTP_PORT)),
    tcpServer.listen(MQTT_PORT)
  ]).then(() => {
    console.log(`Example HTTP API accessible on port ${HTTP_PORT}`)
    console.log(`Example MQTT API accessible on port ${MQTT_PORT}`)
  })
}

/**
 * Stops server.
 */
function stopServers() {
  return Promise.all([
    httpServer.close(),
    tcpServer.close(),
    mqttBroker.close()
  ]).then(() => {
    console.log(`Stopped HTTP API server`)
    console.log(`Stopped MQTT API server`)
  })
}

// If run from command line, start server:
if (require.main === module) {
  startServers(3008, 1885)
}

module.exports = {
  startServers,
  stopServers
}
