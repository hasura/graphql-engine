# ra-data-hasura

> [react-admin](https://github.com/marmelab/react-admin) data provider for Hasura GraphQL Engine

## Installation

```
$ npm install --save ra-data-hasura
```

## Usage

The `ra-data-hasura` provider accepts three arguments:

- `serverEndpoint` - The URL at which Hasura GraphQL Engine is running. (for example: http://localhost:8080). This is required. It should also expose `/v1/query` endpoint.

- `headers` - An optional argument. Pass your auth headers here.

- `config` - An optional argument. Pass your config here.

```
hasuraDataProvider(serverEndpoint, headers, config)
```

In the following example, we import `hasuraDataProvider` from `ra-data-hasura` and give it the hasura server endpoint (assumed to be running at http://localhost:8080) and an optional headers object.

```js
import React from 'react';
import PostIcon from '@material-ui/icons/Book';
import UserIcon from '@material-ui/icons/Group';
import { Admin, Resource, ListGuesser } from 'react-admin';
import hasuraDataProvider from 'ra-data-hasura';

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


