const { promisify } = require('util')
const Knex = require('knex')
const connection = require('../knexfile')
const { Model } = require('objection')
const bcrypt = require('bcryptjs')
const jwt = require('jsonwebtoken')
const jwtConfig = require('../config/jwt')

const knexConnection = Knex(connection)

Model.knex(knexConnection)

class Role extends Model {
  static get tableName() {
    return 'role'
  }

  static get idColumn() {
    return 'id'
  }
}

class User extends Model {
  static get tableName() {
    return 'user'
  }

  static get idColumn() {
    return 'id'
  }

  static get relationMappings() {
    return {
      roles: {
        relation: Model.ManyToManyRelation,
        modelClass: Role,
        join: {
          from: 'user.id',
          through: {
            from: 'user_role.user_id',
            to: 'user_role.role_id'
          },
          to: 'role.id'
        }
      }
    }
  }

  getRoles() {
    return this.roles.map(el => el.name).concat('user')
  }

  getUser() {
    return {
      id: this.id,
      username: this.username,
      roles: this.getRoles(),
      token: this.getJwt()
    }
  }

  getHasuraClaims() {
    return {
      'x-hasura-allowed-roles': this.getRoles(),
      'x-hasura-default-role': 'user',
      'x-hasura-user-id': `${this.id}`
      // 'x-hasura-org-id': '123',
      // 'x-hasura-custom': 'custom-value'
    }
  }

  getJwt() {
    const signOptions = {
      subject: this.id,
      expiresIn: '30d', // 30 days validity
      algorithm: 'RS256'
    }
    const claim = {
      name: this.username,
      // iat: Math.floor(Date.now() / 1000),
      'https://hasura.io/jwt/claims': this.getHasuraClaims()
    }
    return jwt.sign(claim, jwtConfig.key, signOptions)
  }

  async $beforeInsert() {
    const salt = bcrypt.genSaltSync()
    this.password = await bcrypt.hash(this.password, salt)
  }

  async $beforeUpdate() {
    await $beforeInsert()
  }

  verifyPassword(password, callback) {
    bcrypt.compare(password, this.password, callback)
  }

  static get jsonSchema() {
    return {
      type: 'object',
      required: ['username'],
      properties: {
        id: { type: 'integer' },
        username: { type: 'string', minLength: 1, maxLength: 255 }
      }
    }
  }
}

module.exports = { User, Role }
