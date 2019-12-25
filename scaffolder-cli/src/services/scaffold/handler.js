const { getActionScaffold } = require('./scaffold');

const handler = async (payload) => {

  try {
    const scaffolds = await getActionScaffold(payload);
    return JSON.stringify({
      scaffolds
    });
  } catch (e) {
    return JSON.stringify({
      error: e.message
    });
  }

}

module.exports = handler;
