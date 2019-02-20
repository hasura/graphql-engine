exports.up = function(knex, Promise) {
  return knex.schema.createTable('user', table => {
    table
      .uuid('id')
      .primary()
      .unique()
      .defaultTo(knex.raw('gen_random_uuid()'))
    table
      .string('username')
      .unique()
      .notNullable()
    table.string('password').notNullable()
    table
      .timestamp('created_at')
      .notNullable()
      .defaultTo(knex.raw('now()'))
    table
      .boolean('active')
      .defaultTo(true)
      .index()
  })
}

exports.down = function(knex, Promise) {
  return knex.schema.dropTable('user')
}
