import ObjectMapper

class TodoReturningResponse: Mappable {
    
    var affectedRows: Int?
    var returning: [ArticleRecord]?
    
    required init?(map: Map) {
        
    }
    
    func mapping(map: Map) {
        affectedRows <- map["affected_rows"]
        returning <- map["returning"]
    }
}
