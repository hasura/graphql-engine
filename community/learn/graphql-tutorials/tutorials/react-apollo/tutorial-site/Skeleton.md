
# App structure
App.js should have routes inside it. Structure should be as simple and modular and well named as possible.
index.js opens app.js like in create-react-app. Don't change that structure.

- Have one route for handling auth stuff and delegate that to auth0 component
- Have one route for home page
- If logged in -> home
- If not-logged in -> auth

# Setup and first steps
- npm start and check if app is working
- Exercise 1: Go to home page and show the auth0 token on the top banner

# Explore with GraphiQL
- Query public todos
- Use auth token and insert todo
- Use auth token and query your own todos

# Now make todo list work
- Query todo

# Now make insert todo work
- Motivate variables and use GraphQL variables in GraphiQL
- Now insert todo mutation
- Update apollo cache

# Online users
- Use GraphiQL to show online users
- Motivate subscriptions
- Integrate in app

# Online user activty update
- Show mutation with GraphiQL
- setInterval mutation to show online activity

# Realtime feed
- Use GraphiQL to select latest 10 public todos with userinfo as a fragment
- Integrate query in app
- Use GraphiQL to show "Load more" query for older todos
- Integrate "Load more" query in app
- Use GraphiQL to show insert public todo
- Integrate insert public todo in app
- Use GraphiQL to get latest public todo event
- Test with insert public todo in app
- Integrate subscription in app to show banner
- Use GraphiQL to show get latest todos (on banner click)
- Integrate query to fetch latest todos in app


