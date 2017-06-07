iimport ObjectMapper

class MessageResponse: Mappable {
    
    var message: String!
    
    required init?(map: Map) {
        
    }
    
    func mapping(map: Map) {
        message <- map["message"]
    }
}

