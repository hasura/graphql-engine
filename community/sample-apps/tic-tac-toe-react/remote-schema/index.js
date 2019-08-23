const { ApolloServer } = require('apollo-server');
const { makeExecutableSchema } = require('graphql-tools');
const knex = require('knex');

const PG_STRING = process.env.POSTGRES_CONNECTION_STRING;

const knexClient = knex({
  client: 'pg',
  connection: PG_STRING
});

const winMoveMatrix = [
  [0, 1, 2],
  [3, 4, 5],
  [6, 7, 8],
  [0, 3, 6],
  [1, 4, 8],
  [2, 5, 8],
  [0, 4, 8],
  [2, 4, 6],
];

const isWinner = (moves, move, turn, user_1_id, user_2_id) => {
  let gameOver = false;
  const boardState = Array(9).fill();
  moves.forEach(m => {
    boardState[m.position] = m.user_id === user_1_id ? 'x' : 'o';
  })
  boardState[move] = turn;
  for (var i = winMoveMatrix.length - 1; i >= 0; i--) {
    const winMove = winMoveMatrix[i];
    if (!winMove.includes(move)) {
      continue;
    } else {
      if (
        boardState[winMove[0]] === turn &&
        boardState[winMove[1]] === turn &&
        boardState[winMove[2]] === turn
      ) {
        gameOver = true;
        break;
      }
    }
  }
  return gameOver;
};

// Things to check
/*
  1. Correct user id for the board
  2. Correct turn
  3. Correct move (if the move is not at an invalid position)
  4. Switch turn
*/

const handleMove = async (userId, boardId, position) => {
  return knexClient.transaction((trx) => {
    return trx.where({id: boardId})
      .select('turn', 'id', 'user_1_id', 'user_2_id')
      .from('board')
      .then((boardState) => {
        const {
          turn,
          id,
          user_1_id,
          user_2_id
        } = boardState[0];
        if (userId !== user_1_id && userId !== user_2_id) {
          return trx.rollback(new Error('invalid user'));
        }
        if (userId === user_1_id || userId === user_2_id ) {
          if ((userId === user_1_id && turn !== 'x') || (userId === user_2_id && turn !== 'o') ) {
            return trx.rollback(new Error('not the right turn'));
          } else {
            return trx.where({board_id: boardId})
              .from('move')
              .select('user_id', 'position')
              .then((moveState) => {
                if (
                  moveState.find(m => m.position === position) ||
                  parseInt(position) !== position ||
                  position > 8 ||
                  position < 0
                ) {
                  return trx.rollback(new Error('invalid move'));
                }
                const gameOver = isWinner(moveState, position, turn, user_1_id, user_2_id);
                const moveInsertObj = {
                  user_id: userId,
                  board_id: boardId,
                  position
                }
                const boardUpdateSet = {
                  turn: turn === 'x' ? 'o' : 'x',
                  winner: gameOver ? turn : null,
                }
                console.log('Move:');
                console.log(moveInsertObj);
                console.log('Board:');
                console.log(boardUpdateSet);
                return trx('move').insert(moveInsertObj).then(() => {
                  return trx('board').where({id: boardId}).update(boardUpdateSet);
                })
              })
          }
        }
      }).then(() => {
        return { success: true };
      })
      .catch((e) => {
        throw e;
      });
    })
}

const typeDefs = `
  type Query{
    hello: String
  }

  type Mutation {
    make_move (user_id: Int, position: Int, board_id: String!): Result
  }

  type Result {
    success: Boolean,
    message: String
  }
`;

const resolvers = {
  Mutation: {
    make_move: (parent, args) => {
      const { user_id, position, board_id } = args;
      return handleMove(user_id, board_id, position);
    }
  },
  Query: {
    hello: () => "World"
  }
}

let schema;

try {
  const schema = makeExecutableSchema({
    typeDefs,
    resolvers,
    debug: false
  });
} catch (e) {
  console.error('error building schema');
  console.error(e);
}

const server = new ApolloServer({
  typeDefs,
  resolvers,
  debug: false
});

server.listen({port: 4000}).then(({url}) => {
  console.log(`Listening at ${url}`);
});
