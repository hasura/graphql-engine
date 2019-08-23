import UIKit
import Auth0


class TodoVC: UIViewController, UITableViewDelegate, UITableViewDataSource, UITextFieldDelegate, CheckBox {
    
    @IBOutlet weak var todoTable: UITableView!
    @IBOutlet weak var todoFilter: UISegmentedControl!
    @IBOutlet weak var todoInput: UITextField!
    @IBOutlet weak var todoCompleteAction: UIButton!
    
    var todos: [Todo] = []
    var filteredTodos: [Todo] = []
    let empty: String = ""
    
    
    
    override func viewDidAppear(_ animated: Bool) {
        if( SessionManager.shared.credentials?.idToken! == nil ) {
           self.performSegue(withIdentifier: "loginVC", sender: self)
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view.
        
        // Add Dummy Data
        todos.append(Todo(title: "My Private Todo"))
        todos.append(Todo(title: "My Private Todo 1"))
        todos.append(Todo(title: "My Private Todo 2"))
        todos.append(Todo(title: "My Private Todo 3"))
        todos.append(Todo(title: "My Private Todo 4"))
        filteredTodos = todos
        todoTable.reloadData()
        
        setupUI()
        Utils.shared.setLeftPaddingInput(ofInput: todoInput)
        
        // Dynamic Row Height
        todoTable.estimatedRowHeight = 44.0
        todoTable.rowHeight = UITableView.automaticDimension
    }
    
    func textFieldShouldReturn(_ textField: UITextField) -> Bool{
        // Clear text field & remvove keyboard
        if let title = textField.text, title != empty {
            textField.text = empty
            textField.resignFirstResponder()
        }
        return true
    }
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return filteredTodos.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let todoCell = todoTable.dequeueReusableCell(withIdentifier: "todoItem", for: indexPath) as! TodoCell
        todoCell.todoTitle.text = filteredTodos[indexPath.row].title
        if ( filteredTodos[indexPath.row].isChecked ){
            todoCell.todoCheckbox.setBackgroundImage(#imageLiteral(resourceName: "checked"), for: UIControl.State.normal)
        }else {
            todoCell.todoCheckbox.setBackgroundImage(#imageLiteral(resourceName: "unchecked"), for: UIControl.State.normal)
        }
        
        todoCell.delegate = self
        todoCell.todos = filteredTodos
        todoCell.indexP = indexPath.row
        return todoCell
    }
    
    // Swipe Row to Delete
    func tableView(_ tableView: UITableView, commit editingStyle: UITableViewCell.EditingStyle, forRowAt indexPath: IndexPath) {
        if editingStyle == .delete {
        // remove the item from the data model
        self.removeTodos(indexPath: indexPath.row)
        }
    }
    
    @IBAction func todoFilterChange(_ sender: Any) {
        // Do filterting of list on click of segment controller
        filteredTodos = getFilteredTodos(segmentIndex: todoFilter.selectedSegmentIndex)
        todoTable.reloadData()
        
        // Show/hide Complete Action
        toggleCompleteAction()
        
    }
    
    @IBAction func removeAllCompleted(_ sender: Any) {
        // Remove all completed todos on click of Remove Completed Button
    }
    
    // OnClick of Checkbox
    func checkBox(checked: Bool!, index: Int!) {
        filteredTodos[index].isChecked = checked
        todoTable.reloadRows(at: [IndexPath(row: index, section: 0)], with: UITableView.RowAnimation.automatic)
    }
    
    // UI utilities
    func setupUI() {
        if(todos.count > 0){
            todoFilter.isHidden = false
            todoTable.isHidden = false
        } else {
            todoFilter.isHidden = true
            todoFilter.selectedSegmentIndex = 0
            todoTable.isHidden = true
        }
    }
    
    // Complete Action button toggle when on Completed Segment & there are completed items
    func toggleCompleteAction(){
        if(todoFilter.selectedSegmentIndex == 2 && todos.filter({$0.isChecked}).count > 0){
            todoCompleteAction.isHidden = false
        } else {
            todoCompleteAction.isHidden = true
        }
    }
    
    // Filtered Todo list
    func getFilteredTodos(segmentIndex: Int = 0) -> [Todo]{
        switch segmentIndex{
        case 0:
            return self.todos
        case 1:
            return self.todos.filter { $0.isChecked == false }
        case 2:
            return self.todos.filter { $0.isChecked == true }
        default:
            return self.todos
        }
    }
    
    
    // Add a New Todo
    func addTodos(todo: Todo){
    }
    
    // Remove Todos
    func removeTodos(indexPath: Int){
        
    }
}

class Todo {
    var title = ""
    var isChecked = false
    
    convenience init(title: String){
        self.init()
        self.title = title
    }
}



