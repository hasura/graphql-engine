const fs = require("fs");
let collection = process.argv[2];

let rawdata = fs.readFileSync(`/schema_exports/analysis/${collection}.json`);
let analysisData = JSON.parse(rawdata);
let schema = {
  $jsonSchema: {
    bsonType: "object",
    required: [""],
    properties: {},
  },
};
let output = {};
const createTemplate = (obj) => {
  obj.forEach((element) => {
    const keys = element._id.key.split(".");
    const typeKeys = Object.keys(element.value.types);
    let type;
    if (typeKeys.length > 1) {
      type = "string";
    } else {
      type = typeKeys[0].toLowerCase();
    }
    const convertedType = type === "objectid" ? "objectId" : type;

    let subObj = output;
    for (let i = 0; i < keys.length; i++) {
      if (!subObj.properties) {
        subObj.properties = {};
      }

      if (!subObj.properties[keys[i]]) {
        if (i < keys.length - 1 || convertedType === "object") {
          subObj.properties[keys[i]] = {};
        } else {
          subObj.properties[keys[i]] = { bsonType: convertedType };
        }
      }

      if (i === keys.length - 1 && convertedType === "object") {
        subObj.properties[keys[i]].bsonType = convertedType;
        subObj = subObj.properties[keys[i]];
      } else {
        subObj = subObj.properties[keys[i]];
      }
    }
  });
};

createTemplate(analysisData);
schema.$jsonSchema = output;
fs.writeFileSync(
  `/schema_exports/validation_schema/${collection}.json`,
  JSON.stringify(schema, null, 2)
);