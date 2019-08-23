import express from 'express';
import morgan from 'morgan';
import graphqlHTTP from 'express-graphql';

import echo from './echo';
import { schema, root } from './graphql';

const app = express();
app.use(express.json());
app.use(morgan('tiny'));

app.get('/', (req, res) => res.send('api server listening for requests'));

app.post('/trigger/echo', echo);

app.use('/remote-schema/hello', graphqlHTTP({
  schema: schema,
  rootValue: root,
  graphiql: process.env.ENABLE_GRAPHIQL === 'true' ? true : false,
}));

const port = process.env.PORT || 3000;
app.listen(port, () => console.log(`event triggers listening on port ${port}!`));
