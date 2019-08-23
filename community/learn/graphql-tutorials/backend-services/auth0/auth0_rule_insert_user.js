function (user, context, callback) {
  const userId = user.user_id;
  const nickname = user.nickname;
  
  request.post({
  headers: {'content-type' : 'application/json', 'x-hasura-admin-secret': ''},
  url:     'https://learn.hasura.io/graphql',
  body:    `{\"query\":\"mutation($userId: String!, $nickname: String) {\\n          insert_users(\\n            objects: [{ id: $userId, name: $nickname }]\\n            on_conflict: {\\n              constraint: users_pkey\\n              update_columns: [last_seen, name]\\n            }\\n          ) {\\n            affected_rows\\n          }\\n        }\",\"variables\":{\"userId\":\"${userId}\",\"nickname\":\"${nickname}\"}}`
}, function(error, response, body){
    console.log(body);
    callback(null, user, context);
});
  
  
}