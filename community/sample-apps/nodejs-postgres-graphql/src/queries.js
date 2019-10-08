const Pool = require("pg").Pool;
const pool = new Pool({
  connectionString: process.env.DB_URI
});

const testDB = (request, response) => {
  pool.query('SELECT NOW()', (err, res) => {
    if (err) {
      console.error(err)
      response.status(500).json(err);
    }
    console.info(res)
    response.status(200).json(res);
  })
}

module.exports = {
  testDB
};
