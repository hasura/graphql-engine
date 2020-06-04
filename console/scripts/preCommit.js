require('dotenv').config();

const execSync = require('child_process').execSync;

if (process.env.HUSKY_PRE_COMMIT) {
  execSync('node_modules/.bin/lint-staged', { stdio: 'inherit' });
}
