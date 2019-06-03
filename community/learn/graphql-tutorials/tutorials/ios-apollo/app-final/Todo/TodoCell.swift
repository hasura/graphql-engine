import UIKit

protocol CheckBox {
    func checkBox(checked: Bool, index: Int!)
}

class TodoCell: UITableViewCell {

    
    @IBOutlet weak var todoCheckbox: UIButton!
    @IBOutlet weak var todoTitle: UILabel!
    
    var indexP: Int?
    var delegate: CheckBox?
    var todos: [GetMyTodosQuery.Data.Todo]?
    
    override func awakeFromNib() {
        super.awakeFromNib()
        // Initialization code
    }

    override func setSelected(_ selected: Bool, animated: Bool) {
        super.setSelected(selected, animated: animated)
        // Configure the view for the selected state
    }
   
    @IBAction func todoCheckboxAction(_ sender: UIButton) {
        if todos![indexP!].isCompleted {
            delegate?.checkBox(checked: false, index: indexP)
        } else {
            delegate?.checkBox(checked: true, index: indexP)
        }
    }
    
    
}
