const toPayload = {
  "action": {
    "arguments": [{
      "name": "user",
      "type": "UserInput",
      "description": "lolz"
    }],
    "output_type": "[UserInfo!]",
    "name": "validatedUserInsert"
  },
  "types": {
     "enums": [],
     "scalars": [],
     "input_objects": [
      {
        "name": "UserInput",
        "fields": [
          {
            "name": "username",
            "type": "String",
            "description": "lalz"  
          },
          {
            "name": "password",
            "type": "String!",
            "description": "pass"  
          }
        ]
      }
     ],
     "objects": [
      {
        "name": "UserInfo",
        "fields": [
          {
            "name": "accessToken",
            "type": "String",
            "description": "lolz"  
          }
        ] 
      }
     ]
  }
};

const fromPayload = {
  "sdl": {
    "action": "type Mutation { actionName (arg1: SampleInput!): SampleOutput }",
    "types": "type SampleOutput { accessToken: String! } input SampleInput { username: String! password: String! }"
  },
  "types": {
    "scalars": [],
    "enums": [],
    "input_objects": [],
    "objects": []
  }
};

module.exports = {
  toPayload,
  fromPayload
}