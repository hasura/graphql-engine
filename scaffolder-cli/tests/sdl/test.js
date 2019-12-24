const { toPayload, fromPayload } = require('./payload');

const { handlePayload: toHandler  } = require('../../build/services/sdl/to/handler');
const { handlePayload: fromHandler } = require('../../build/services/sdl/from/handler');

const test = async () => {

  const toResponse = toHandler(toPayload);
  if (
    toResponse.status === 200 &&
    toResponse.body.sdl.complete === "type Mutation {\n  actionName (\n    input: [ActionNameUserInsertInput!]!\n  ): ActionNameUser\n}\n\n\n\ninput ActionNameUserInsertInput {\n  id : Int\n  name : String\n  nullable : String\n}\n\ntype ActionNameUser {\n  id : Int!\n  name : String!\n  nullable : String\n}\n\n\n\ntype Mutation {\n  validatedUserInsert (\n    user: UserInput\n  ): [UserInfo!]\n}\n\n\n\n\ninput UserInput {\n  username : String\n  password : String!\n}\n\ntype UserInfo {\n  accessToken : String\n}\n\n"
  ) {
    console.log('✓ Conversion from metadata to SDL passed');
  } else {
    console.log('✘ Conversion from metadata to SDL failed');
    console.log(toResponse);
    process.exit(1);
  }

  const fromResponse = fromHandler(fromPayload);
  if (
    fromResponse.status === 200 &&
    fromResponse.body.actions.length === 2 &&
    fromResponse.body.actions[0].name === 'actionName1' &&
    fromResponse.body.actions[1].name === 'actionName2'
  ) {
    console.log('✓ Conversion from SDL to metadata passed');
  } else {
    console.log('✘ Conversion from SDL to metadata failed');
    console.log(fromResponse);
    process.exit(1);
  }

  return Promise.resolve()

}

module.exports = test;
