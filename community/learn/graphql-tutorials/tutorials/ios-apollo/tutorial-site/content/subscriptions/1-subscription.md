---
title: "Subscription"
metaTitle: "Set up GraphQL Subscriptions using Apollo Client | GraphQL iOS Apollo Tutorial"
metaDescription: "You will learn how to configure GraphQL Subscriptions using iOS Apollo Client by installing dependencies like apollowebsocket.framework, starscream.framework. This will also have authorization token setup"
---

import GithubLink from '../../src/GithubLink.js'

When we had initially set up Apollo, we used just the network transport. But subscriptions is an advanced use case which needs more configuration. So we have to add ApolloWebSocket. Add `apollowebsocket.framework` and `starscream.framework` from your Carthage/Build/IOS to libraries.

Now we need to update our `ApolloClient` instance to point to the subscription server.

Open `Todo/NetworkManager.swift` and update the following:

<GithubLink link="https://github.com/hasura/graphql-engine/blob/master/community/learn/graphql-tutorials/tutorials/ios-apollo/app-final/Todo/NetworkManager.swift" text="Todo/NetworkManager.swift" />

```swift
import Foundation
import Apollo
+ import ApolloWebSocket

class NetworkManager {
    static let shared = NetworkManager()
    let graphEndpoint = "https://learn.hasura.io/graphql"
+    let graphWSEndpoint = "wss://learn.hasura.io/graphql"
    var apolloClient : ApolloClient?
    
    private init (){
    }
    
    func setApolloClient(accessToken: String){
        self.apolloClient = {
            let authPayloads = ["Authorization": "Bearer \(accessToken)"]
            let configuration = URLSessionConfiguration.default
            configuration.httpAdditionalHeaders = authPayloads
            
+            let map: GraphQLMap = authPayloads
+            let wsEndpointURL = URL(string: graphWSEndpoint)!
            let endpointURL = URL(string: graphEndpoint)!
+            var request = URLRequest(url: wsEndpointURL)
+            request.setValue("Bearer \(accessToken)", forHTTPHeaderField: "Authorization")
+            let websocket = WebSocketTransport(request: request, sendOperationIdentifiers: false, reconnectionInterval: 30000, connectingPayload: map)
+            let splitNetworkTransport = SplitNetworkTransport(
+                httpNetworkTransport: HTTPNetworkTransport(
+                    url: endpointURL,
+                    configuration: configuration
+                ),
+                webSocketNetworkTransport: websocket
+            )
-            return ApolloClient(networkTransport: HTTPNetworkTransport(url: endpointURL, configuration: configuration))
+            return ApolloClient(networkTransport: splitNetworkTransport)
            }()
    }

}
```

Note that we are replacing `HTTPNetworkTransport` with `SplitNetworkTransport` and hence all GraphQL queries go through a single websocket connection.
