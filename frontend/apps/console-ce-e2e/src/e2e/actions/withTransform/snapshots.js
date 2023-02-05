module.exports = {
  "__version": "10.11.0",
  "Actions with Transform": {
    "When the users create, and delete a Action with Transform, everything should work": {
      "Action payload": {
        "bodyToSnapshot": [
          {
            "type": "set_custom_types",
            "args": {
              "scalars": [],
              "input_objects": [],
              "objects": [
                {
                  "name": "LoginResponse",
                  "description": null,
                  "fields": [
                    {
                      "name": "accessToken",
                      "type": "String!",
                      "description": null
                    }
                  ]
                }
              ],
              "enums": []
            }
          },
          {
            "type": "create_action",
            "args": {
              "name": "login",
              "definition": {
                "arguments": [
                  {
                    "name": "username",
                    "type": "String!",
                    "description": null
                  },
                  {
                    "name": "password",
                    "type": "String!",
                    "description": null
                  }
                ],
                "kind": "synchronous",
                "output_type": "LoginResponse",
                "handler": "https://hasura-actions-demo.glitch.me",
                "type": "mutation",
                "headers": [],
                "forward_client_headers": false,
                "timeout": null,
                "request_transform": {
                  "version": 2,
                  "template_engine": "Kriti",
                  "method": "POST",
                  "url": "{{$base_url}}/{{$body.action.name}}",
                  "query_params": {
                    "id": "5",
                    "name": "{{$body.action.name}}"
                  },
                  "body": {
                    "action": "transform",
                    "template": "{\n              \"userInfo\": {\n                              \"name\": {{$body.input.username}},\n                                            \"password\": {{$body.input.password}},\n                                                          \"type\": {{$body.action.name}}\n                                                                      \n              }\n}"
                  }
                },
                "response_transform": {
                  "version": 2,
                  "body": {
                    "action": "transform",
                    "template": "{\n              \"userInfo\": {\n                              \"name\": {{$body.input.username}},\n                                            \"password\": {{$body.input.password}},\n                                                          \"type\": {{$body.action.name}}\n                                                                      \n              }\n}"
                  },
                  "template_engine": "Kriti"
                }
              },
              "comment": null
            }
          }
        ]
      },
      "Action metadata": {
        "name": "login",
        "definition": {
          "handler": "https://hasura-actions-demo.glitch.me",
          "output_type": "LoginResponse",
          "arguments": [
            {
              "name": "username",
              "type": "String!"
            },
            {
              "name": "password",
              "type": "String!"
            }
          ],
          "request_transform": {
            "body": {
              "action": "transform",
              "template": "{\n              \"userInfo\": {\n                              \"name\": {{$body.input.username}},\n                                            \"password\": {{$body.input.password}},\n                                                          \"type\": {{$body.action.name}}\n                                                                      \n              }\n}"
            },
            "method": "POST",
            "query_params": {
              "id": "5",
              "name": "{{$body.action.name}}"
            },
            "template_engine": "Kriti",
            "url": "{{$base_url}}/{{$body.action.name}}",
            "version": 2
          },
          "response_transform": {
            "body": {
              "action": "transform",
              "template": "{\n              \"userInfo\": {\n                              \"name\": {{$body.input.username}},\n                                            \"password\": {{$body.input.password}},\n                                                          \"type\": {{$body.action.name}}\n                                                                      \n              }\n}"
            },
            "template_engine": "Kriti",
            "version": 2
          },
          "type": "mutation",
          "kind": "synchronous"
        }
      }
    }
  }
}
