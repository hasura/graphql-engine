const samplePayload = {
    "action": {
        "action_defn": {
           "arguments": [
              {
                  "name": "arg1",
                  "type": "SampleInput!"
              }
            ],
            "output_type": "SampleOutput"
        },
        "name": "actionName"
    },
    "types": {
        "scalars": [],
        "input_objects": [
            {
                "name": "SampleInput",
                "fields": [
                    {
                        "name": "username",
                        "type": "String!"
                    },
                    {
                        "name": "password",
                        "type": "String!"
                    }
                ]
            }
        ],
        "objects": [
            {
                "name": "SampleOutput",
                "fields": [
                    {
                        "name": "accessToken",
                        "type": "String!"
                    }
                ]
            }
        ],
        "enums": []
    },
    "framework": "typescript-express"
};

module.exports = {
  samplePayload
};
