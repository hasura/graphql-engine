We'll start building the backend integration for the basic components of this todo app:

- Fetching and displaying todos
- Inserting a new todo.

The UI boilerplate that you cloned will have the react specific code and styles.

Components are already split for you to get started with. Navigate to `src/components/` to look at the code structure. We have the following components for the overall app functionality:

- `Auth`, for Auth0 integration
- `Callback`, for handling callback from Auth0 after authentication
- `Home`, the landing page with Login
- `OnlineUsers`, component to handle online users list with subscriptions.
- `Todo`, all todo related functionality goes into this folder, with components split for modularity.

All the styles for the app is in `src/styles/App.css`.

The focus is on GraphQL integration and hence you can get just familiar enough with the current code that you have to get started quickly.

