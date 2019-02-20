const express = require("express");
const serverless = require("serverless-http");
const isProduction = process.env.NODE_ENV === "production";

// setup Express and hook up Next.js handler
const app = express();

app.get('/', require('./.next/serverless/pages/index').render)
app.get('*', (req, res) => {
  const  pathname  = req.url
  try {
    require(`./.next/serverless/pages${pathname}`).render(req, res)
  } catch (err) {
    require('./.next/serverless/pages/_error').render(req, res)
  }
})

// for local development (serverless offline)
if (!isProduction) {
	// host the static files
	app.use("/_next/static", express.static("./.next/static"));
	app.use("/static", express.static("./static"));
}

// 404 handler
app.get("*", require("./.next/serverless/pages/_error").render);

// export the wrapped handler for the Lambda runtime
exports.handler = serverless(app);
