const { getActionsCodegen  } = require('./codegen');

const handler = async (payload) => {

  try {
    const codegen = await getActionsCodegen(payload);
    return JSON.stringify({
      codegen
    });
  } catch (e) {
    return JSON.stringify({
      error: e.message
    });
  }

}

module.exports = handler;
