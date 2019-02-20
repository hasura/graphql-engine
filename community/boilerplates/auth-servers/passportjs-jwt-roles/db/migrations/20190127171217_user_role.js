exports.up = function(knex, Promise) {
  return knex.schema.createTable('user_role', table => {
    table
      .uuid('id')
      .primary()
      .unique()
      .defaultTo(knex.raw('gen_random_uuid()'))
    table
      .uuid('role_id')
      .unsigned()
      .index()
      .references('id')
      .inTable('role')
    table
      .uuid('user_id')
      .unsigned()
      .index()
      .references('id')
      .inTable('user')
  })
}

exports.down = function(knex, Promise) {
  return knex.schema.dropTable('user_role')
}
