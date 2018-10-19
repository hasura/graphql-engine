exports.handler = async (event) => {
  let response = {}
  try {
    let { table: { name }, event: { op, data } } = event.body;
    response.statusCode = 200;
    if (name === "notes" && op === "INSERT") {
      response.body = `New note ${data.new.id} inserted, with data: ${data.new.note}`;
    }
    else if (name === "notes" && op === "UPDATE") {
      response.body = `Note ${data.new.id} updated, with data: ${data.new.note}`;
    }
    else if (name === "notes" && op === "DELETE") {
      response.body = `Note ${data.old.id} deleted, with data: ${data.old.note}`;
    }
    return response
  } catch (e) {
    response.statusCode = 400;
    response.body = "cannot parse hasura event";
    return response
  }
};