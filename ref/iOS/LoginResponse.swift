import Foundation
import ObjectMapper

class AuthResponse: Mappable {
    
    var authToken: String?
    var id: Int!
    var roles: [String]!
    
    required init?(map: Map) {
        
    }
    
    func mapping(map: Map) {
        authToken <- map["auth_token"]
        id <- map["hasura_id"]
        roles <- map["hasura_roles"]
    }
    
}
