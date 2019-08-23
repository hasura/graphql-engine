/**
 * Module dependencies.
 */
require('dotenv').config();

const express = require('express');
const bodyParser = require('body-parser');
const chalk = require('chalk');
const dotenv = require('dotenv');
const passport = require('passport');
const expressValidator = require('express-validator');
const cors = require('cors');
const cloudinary = require('cloudinary');
const multer = require('multer');
const tmp = require('tmp');

/**
 * Load environment variables from .env file, where API keys and passwords are configured.
 */
dotenv.load({ path: '.env' });

if(!process.env.ENCRYPTION_KEY) {
  throw new Error('JWT encryption key required')
}

/**
 * Controllers (route handlers).
 */

/**
 * Create Express server.
 */
const app = express();
/**
 * Express configuration.
 */
app.use(cors());
app.set('host', '0.0.0.0');
app.set('port', process.env.PORT || 8080);
app.set('json spaces', 2); // number of spaces for indentation
app.use(bodyParser.json());
app.use(expressValidator());
app.use(passport.initialize());
app.use(passport.session());

const userController = require('./controllers/user');

const upload = multer({
  dest: tmp.dirSync({ unsafeCleanup: true }).name,
})
const uploadProfilePic = (filePath) => {
	return new Promise((resolve, reject) => {
	  cloudinary.v2.uploader.upload(filePath, (error, result) => {
	    if (error) {
	      reject(error)
	    } else {
	      resolve(result)
	    }
	  })
	})
}

app.post('/login', userController.postLogin);
app.post('/signup', userController.postSignup);
app.post('/upload-profile-pic', upload.single('file'), async (req, res, done) => {
  try {
    res.json(await uploadProfilePic(req.file.path))
  } catch (e) {
    done(e)
  }
})

/**
 * Start Express server.
 */
app.listen(app.get('port'), () => {
  console.log('%s App is running at http://localhost:%d in %s mode', chalk.green('✓'), app.get('port'), app.get('env'));
  console.log('  Press CTRL-C to stop\n');
});

module.exports = app;
