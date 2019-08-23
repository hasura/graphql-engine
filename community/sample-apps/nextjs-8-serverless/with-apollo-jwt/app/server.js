const { createServer } = require('http')
const httpProxy = require('http-proxy')
const { parse } = require('url')
const next = require('next')

const dev = process.env.NODE_ENV !== 'production'
const app = next({ dev })
const handle = app.getRequestHandler()

const proxy = httpProxy.createProxyServer()
const target = 'http://localhost:8080'

app.prepare().then(() => {
  createServer((req, res) => {
    const parsedUrl = parse(req.url, true)
    const { pathname, query } = parsedUrl

    switch (pathname) {
      case '/':
        app.render(req, res, '/', query)
        break

      case '/sign-in':
        app.render(req, res, '/login', query)
        break

      case '/sign-up':
        app.render(req, res, '/signup', query)
        break

      case '/login':
        proxy.web(req, res, { target }, error => {
          console.log('Error!', error)
        })
        break

      case '/signup':
        proxy.web(req, res, { target }, error => {
          console.log('Error!', error)
        })
        break

      case '/articles':
        app.render(req, res, '/articles', query)
        break

      case '/api/profile.js':
        proxy.web(req, res, { target }, error => console.log('Error!', error))
        break

      default:
        handle(req, res, parsedUrl)
        break
    }
  }).listen(3000, err => {
    if (err) throw err
    console.log('> Ready on http://localhost:3000')
  })
})
