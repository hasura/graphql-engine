# ra-data-hasura

> [react-admin](https://github.com/marmelab/react-admin) data provider for Hasura GraphQL Engine

## Installation

```
$ npm install --save ra-data-hasura
```

## Usage

The `ra-data-hasura` provider accepts three arguments:

- `serverEndpoint` - The URL at which Hasura GraphQL Engine is running. (for example: http://localhost:8080). This is required. It should also expose `/v1/query` endpoint.

- `httpClient` - HTTP Client function. To maintain backwards compatibility the `headers` object is supported.

- `config` - An optional argument. Pass your config here.

```
hasuraDataProvider(serverEndpoint, httpClient, config)
```

In the following example, we import `hasuraDataProvider` from `ra-data-hasura` and give it the hasura server endpoint (assumed to be running at http://localhost:8080) and an optional headers object.

```js
import React from 'react';
import PostIcon from '@material-ui/icons/Book';
import UserIcon from '@material-ui/icons/Group';
import { Admin, Resource, ListGuesser } from 'react-admin';
import hasuraDataProvider from 'ra-data-hasura';

// The following components are created when following the react-admin tutorial
import { PostList, PostEdit, PostCreate, PostShow } from './posts';
import { UserList } from './users';
import Dashboard from './Dashboard';
import authProvider from './authProvider';

const headers = {'content-type': 'application/json', 'authorization': 'bearer <token>'};
const App = () => (
  <Admin
    dataProvider={hasuraDataProvider('http://localhost:8080', headers)}
    authProvider={authProvider}
    dashboard={Dashboard}
  >
    <Resource
      name="posts"
      icon={PostIcon}
      list={PostList}
      edit={PostEdit}
      create={PostCreate}
      show={PostShow}
    />
    <Resource name="users" icon={UserIcon} list={UserList} />
    <Resource name="comments" list={ListGuesser} />
  </Admin>
);

export default App;

```

In case the server is configured with admin secret or auth, configure the appropriate headers and pass it to the provider.

### Adding Custom Headers

The above example showed a simple use case of adding static headers. In order to update headers dynamically, the data provider accepts an HTTP client function as the second argument. It uses react-admin's fetchUtils.fetchJson() as HTTP client. Hence to add custom headers to your requests, you just need to wrap the `fetchUtils.fetchJson()` call inside your own function: 

```javascript
const httpClient = (url, options = {}) => {
  if (!options.headers) {
      options.headers = new Headers({ Accept: 'application/json' });
  }
  // add your own headers here
  options.headers.set('Authorization', 'Bearer xxxxx');
  return fetchUtils.fetchJson(url, options);
};
const dataProvider = hasuraDataProvider('http://localhost:8080', httpClient);
```

### Multiple schemas

To query schemas other than `public`, you can pass schema to resource in the format
 `<Resource name="schema.table" />`.

For example to fetch data from schema `test` and table `author`, use the following snippet:

```
  <Resource name="test.author" list={list} />
```

### Different Primary Keys

Sometimes the table you are querying might have a primary key other than `id`. `react-admin` enforces `id` to be returned in the response by the DataProvider. But you can configure a different primary key column for specific tables using the config object as below:

```
const config = { 
  'primaryKey': { 
      'tableName': 'primaryKeyColumn', 'tableName2': 'primaryKeyColumn' 
  } 
};
```

## Known Issues

Filter as you type (search) functionality inside tables is not supported right now. It is a work in progress.

## Contributing

To modify, extend and test this package locally,

```
$ cd ra-data-hasura
$ npm run link
```

Now use this local package in your react app for testing
```
$ cd my-react-app
$ npm link ra-data-hasura
```

Build the library by running `npm run build` and it will generate the transpiled version of the library under `lib` folder.


