import Foundation
import Apollo
import ApolloWebSocket

class NetworkManager {
    static let shared = NetworkManager()
    let graphEndpoint = "https://learn.hasura.io/graphql"
    let graphWSEndpoint = "wss://learn.hasura.io/graphql"
    var apolloClient : ApolloClient?
    
    private init (){
    }
    
    func setApolloClient(accessToken: String){
        self.apolloClient = {
            let authPayloads = ["Authorization": "Bearer \(accessToken)"]
            let configuration = URLSessionConfiguration.default
            configuration.httpAdditionalHeaders = authPayloads
            
            let map: GraphQLMap = authPayloads
            let wsEndpointURL = URL(string: graphWSEndpoint)!
            let endpointURL = URL(string: graphEndpoint)!
            var request = URLRequest(url: wsEndpointURL)
            request.setValue("Bearer \(accessToken)", forHTTPHeaderField: "Authorization")
            let websocket = WebSocketTransport(request: request, sendOperationIdentifiers: false, reconnectionInterval: 30000, connectingPayload: map)
            let splitNetworkTransport = SplitNetworkTransport(
                httpNetworkTransport: HTTPNetworkTransport(
                    url: endpointURL,
                    configuration: configuration
                ),
                webSocketNetworkTransport: websocket
            )
            return ApolloClient(networkTransport: splitNetworkTransport)
            }()
    }

}
