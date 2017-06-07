import ObjectMapper

class ArticleRecord: Mappable {
    
    var id: Int!
    var name: String!
    var userId: Int!
    
    required init?(map: Map) {
        
    }
    
    func mapping(map: Map) {
        id <- map["id"]
        name <- map["name"]
        userId <- map["user_id"]
    }
}
