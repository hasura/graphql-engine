const { promisify } = require('util');
const Knex = require('knex');
const connection = require('../knexfile');
const { Model } = require('objection');
const bcrypt = require('bcrypt');
const crypto = require('crypto');

const knexConnection = Knex(connection);

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
      'id': this.id
    }
  }

  async $beforeInsert () {
    const salt = bcrypt.genSaltSync();
    this.password = await bcrypt.hash(this.password, salt)
  }

  verifyPassword (password, callback) {
    bcrypt.compare(password, this.password, callback)
  };

  static get jsonSchema () {
    return {
      type: 'object',
      required: ['id'],
      properties: {
        id: {type: 'string', minLength: 1, maxLength: 255},
      }
    }
  }
}

module.exports = { User }
