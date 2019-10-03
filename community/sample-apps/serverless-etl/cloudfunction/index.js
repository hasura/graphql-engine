const algoliasearch = require('algoliasearch');

exports.function = async (req, res) => {
  // webhook payload
  const { event: { op, data }, table: { schema, name } } = req.body;

  // env vars
  const ALGOLIA_APP_ID = process.env.ALGOLIA_APP_ID;
  const ALGOLIA_ADMIN_API_KEY = process.env.ALGOLIA_ADMIN_API_KEY;

  var client = algoliasearch(ALGOLIA_APP_ID, ALGOLIA_ADMIN_API_KEY);
  var index = client.initIndex('demo_serverless_etl_app');


  if (op === 'INSERT' && name === 'book') {
    index.addObjects([data.new], function(err, content) {
      if (err) {
        console.error(err);
        res.json({error: true, data: err});
        return;
      }
      console.log(content);
      res.json({error: false, data: content});
    });
  } else {
    // ignore if the trigger name is not matched
    res.json({error: false, data: {message: 'ignored event'}});
  }
};
