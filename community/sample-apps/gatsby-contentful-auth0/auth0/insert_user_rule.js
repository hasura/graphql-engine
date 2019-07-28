function userSyncRule(user, context, callback) {
  const userId = user.user_id;
  const nickname = user.nickname;

  const mutation = `mutation($userId: String!, $nickname: String) {
    insert_users(objects: [{
        id: $userId,
        name: $nickname
      }],
      on_conflict: {
        constraint: users_pkey,
        update_columns: [name]
      }) {
        affected_rows
      }
    }`;

  request.post(
    {
      headers: {
        "content-type": "application/json",
        "x-hasura-admin-secret": configuration.ADMIN_SECRET
      },
      url: "https://<your-app-domain>/v1/graphql",
      body: JSON.stringify({ query: mutation, variables: { userId, nickname } })
    },
    function(error, response, body) {
      console.log(body);
      callback(error, user, context);
    }
  );
}