const { getActionsCodegen } = require('./codegen');

const handler = async (payload: unknown) => {
  try {
    const codegen = await getActionsCodegen(payload);
    return { codegen };
  } catch (e) {
    return { error: e.message };
  }
};

module.exports = handler;
