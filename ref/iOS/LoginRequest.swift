import Foundation
import ObjectMapper

class AuthRequest: Mappable {
    
    var username: String!
    var password: String!
    
    required init?(map: Map) {
        
    }
    
    public init(username: String, password: String) {
        self.username = username
        self.password = password
    }
    
    func mapping(map: Map) {
        username <- map["username"]
        password <- map["password"]
    }
}
