---
title: "Set up a GraphQL client with Apollo"
metaTitle: "Apollo Client GraphQL Setup | GraphQL iOS Apollo Tutorial"
metaDescription: "You will learn how to configure Apollo Client in iOS by installing dependencies like apollo-ios in Cartfile"
---

import GithubLink from '../src/GithubLink.js'

Apollo gives a neat abstraction layer and an interface to your GraphQL server. You don't need to worry about constructing your queries with request body, headers and options, that you might have done with `AFNetworking` or `NWConnection` say. You can directly write queries and mutations in GraphQL and they will automatically be sent to your server via your apollo client instance.

## iOS Apollo Installation

Let's get started by installing apollo client framework & peer graphql dependencies:

- Add github `"apollostack/apollo-ios"` to your Cartfile.
- Run `carthage update`
- Drag and drop Apollo.framework from the Carthage/Build/iOS folder to the "Linked Frameworks and Libraries" section of your application targets' "General" settings tab
- On your application targets’ "Build Phases" settings tab, click the "+" icon and choose "New Run Script Phase". Create a Run Script in which you specify your shell (ex: `bin/sh`), add the following contents to the script area below the shell:

```sh
/usr/local/bin/carthage copy-frameworks
```

and add the paths to the frameworks you want to use under "Input Files", e.g.:

```sh
$(SRCROOT)/Carthage/Build/iOS/Apollo.framework
```

### Adding a code generation build step

In order to invoke `apollo` as part of the Xcode build process, create a build step that runs before "Compile Sources".

1. On your application targets’ "Build Phases" settings tab, click the "+" icon and choose "New Run Script Phase". Create a Run Script, change its name to "Generate Apollo GraphQL API" and drag it just above "Compile Sources". Then add the following contents to the script area below the shell:

for iOS Project

```sh
APOLLO_FRAMEWORK_PATH="$(eval find $FRAMEWORK_SEARCH_PATHS -name "Apollo.framework" -maxdepth 1)"

if [ -z "$APOLLO_FRAMEWORK_PATH" ]; then
  echo "error: Couldn't find Apollo.framework in FRAMEWORK_SEARCH_PATHS; make sure to add the framework to your project."
  exit 1
fi
```

The script above will invoke `apollo` through the `check-and-run-apollo-cli.sh` wrapper script, which is actually contained in the `Apollo.framework` bundle. The main reason for this is to check whether the version of `apollo` installed on your system is compatible with the framework version installed in your project, and to warn you if it isn't. Without this check, you could end up generating code that is incompatible with the runtime code contained in the framework.

### Adding a schema file to your target directory

You'll have to copy or download a schema to your target directory before generating code.

Apollo iOS requires a GraphQL schema file as input to the code generation process. A schema file is a JSON file that contains the results of an an introspection query. Conventionally this file is called `schema.json`

To download `schema.json`, you need to use the id_token from auth0 and run this in your terminal

```sh
apollo schema:download --endpoint=http://learn.hasura.io/graphql --header="Authorization: Bearer <token>"
```

### Build your target

At this point, you can try building your target in Xcode. This will verify that the `schema.json` file can be found by the `apollo` script created above, otherwise you'll get a build error such as:

> Cannot find GraphQL schema file [...]

### Adding the generated API file to your target

1. Drag the generated `API.swift` file to your target.

> Note that because Apollo iOS generates query-specific result types, `API.swift` will be mostly empty at this point unless you've already added some `.graphql` files with queries or mutations to your target directory.

### Installing the Xcode add-ons to get syntax highlighting

1. Clone the [`xcode-apollo` repository](https://github.com/apollostack/xcode-apollo) to your computer.
1. Close Xcode if it is currently running.
1. You may need to create these folders inside of `~/Library/Developer/Xcode`:

`mkdir ~/Library/Developer/Xcode/Plug-ins ~/Library/Developer/Xcode/Specifications`

1. Copy `GraphQL.ideplugin` to `~/Library/Developer/Xcode/Plug-ins`.

`cp -R GraphQL.ideplugin ~/Library/Developer/Xcode/Plug-ins`

1. Copy `GraphQL.xclangspec` to `~/Library/Developer/Xcode/Specifications`.

`cp -R GraphQL.xclangspec ~/Library/Developer/Xcode/Specifications`

You may receive a warning when you first start up Xcode after installing these add-ons.

### Create .graphql files with your queries or mutations

Apollo iOS generates code from queries and mutations contained in `.graphql` files in your target.

A useful convention is to colocate queries, mutations or fragments with the Swift code that uses them by creating `<name>.graphql` next to `<name>.swift`.

If you have the Xcode add-ons installed, you can use the Xcode companion view to show a `.swift` file and the corresponding `.graphql` file side by side.

### Create apollo client

Open LoginVC.swift and add below

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/ios-apollo/app-final/Todo/LoginVC.swift" text="LoginVC.swift"/>

```swift
case .success(let credentials):
                    if(!SessionManager.shared.store(credentials: credentials)) {
                        print("Failed to store credentials")
                    } else {
                        SessionManager.shared.retrieveProfile { error in
                            DispatchQueue.main.async {
                                guard error == nil else {
                                    print("Failed to retrieve profile: \(String(describing: error))")
                                    return self.showLoginWithPatch()
                                }
+                               NetworkManager.shared.setApolloClient(accessToken: credentials.idToken!)
                                self.dismiss(animated: true, completion: nil)
                            }
                        }
                    }
                }
```

```
DispatchQueue.main.async {
                    SessionManager.shared.retrieveProfile { error in
                        DispatchQueue.main.async {
                            guard error == nil else {
                                print("Failed to retrieve profile: \(String(describing: error))")
                                return callback()
                            }
+                            NetworkManager.shared.setApolloClient(accessToken: (SessionManager.shared.credentials?.idToken!)!)
                            self.dismiss(animated: true, completion: nil)
                        }
                    }
                }
```

We are passing our tokenID to be set in apollo client so that we can make authorised calls to our graphql backend. Now let's create this file which will return apollo client with httplink and cache. Lets call this file 'NetworkManager.swift`

```swift
import Foundation
import Apollo
import ApolloWebSocket

class NetworkManager {
    static let shared = NetworkManager()
    let graphEndpoint = "https://learn.hasura.io/graphql"
    var apolloClient : ApolloClient?

    private init (){
    }

    func setApolloClient(accessToken: String){
        self.apolloClient = {
            let authPayloads = ["Authorization": "Bearer \(accessToken)"]
            let configuration = URLSessionConfiguration.default
            configuration.httpAdditionalHeaders = authPayloads
            let endpointURL = URL(string: graphEndpoint)!

            return ApolloClient(networkTransport: HTTPNetworkTransport(url: endpointURL, configuration: configuration))
            }()
    }

}

```
