const { promisify } = require('util');
const Knex = require('knex');
const connection = require('../knexfile');
const { Model } = require('objection');
const bcrypt = require('bcrypt');
const crypto = require('crypto');

const knexConnection = Knex(connection);
const randomBytesAsync = promisify(crypto.randomBytes);

Model.knex(knexConnection);

class User extends Model {
  static get tableName () {
    return 'users'
  }

  static get idColumn() {
    return 'id';
  }

  getUser() {
    return {
      'id': this.id,
      'username': this.username,
      'token': this.token
    }
  }

  async $beforeInsert () {
    const salt = bcrypt.genSaltSync();
    this.password = await bcrypt.hash(this.password, salt)
    const createRandomToken = await randomBytesAsync(16).then(buf => buf.toString('hex'));
    this.token = createRandomToken
  }

  verifyPassword (password, callback) {
    bcrypt.compare(password, this.password, callback)
  };

  static get jsonSchema () {
    return {
      type: 'object',
      required: ['username'],
      properties: {
        id: {type: 'integer'},
        username: {type: 'string', minLength: 1, maxLength: 255},
        token: {type: 'string', minLength: 1, maxLength: 255}
      }
    }
  }
}

module.exports = { User }
