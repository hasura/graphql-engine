import UIKit
import Auth0
import Apollo


class TodoVC: UIViewController, UITableViewDelegate, UITableViewDataSource, UITextFieldDelegate, CheckBox {
    
    @IBOutlet weak var todoTable: UITableView!
    @IBOutlet weak var todoFilter: UISegmentedControl!
    @IBOutlet weak var todoInput: UITextField!
    @IBOutlet weak var todoCompleteAction: UIButton!
    @IBOutlet weak var loadingIndicator: UIActivityIndicatorView!
    @IBOutlet weak var loadingTitle: UILabel!
    
    var todos: [GetMyTodosQuery.Data.Todo] = []
    var filteredTodos: [GetMyTodosQuery.Data.Todo] = []
    let empty: String = ""
    var apollo: ApolloClient!
    var todoWatcher : GraphQLQueryWatcher<GetMyTodosQuery>!
    
    override func viewWillAppear(_ animated: Bool) {
        if( SessionManager.shared.credentials?.idToken! != nil ) {
            apollo = NetworkManager.shared.apolloClient
            
            // Initialize a Watcher to check for updates
            todoWatcher = todoQueryWatcher()
        }
    }
    
    override func viewWillDisappear(_ animated: Bool) {
            todoWatcher?.cancel()
    }
    
    override func viewDidAppear(_ animated: Bool) {
        if( SessionManager.shared.credentials?.idToken! == nil ) {
            self.performSegue(withIdentifier: "loginVC", sender: self)
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view.
        Utils.shared.setLeftPaddingInput(ofInput: todoInput)
        
        // Dynamic Row Height
        todoTable.estimatedRowHeight = 72.0
        todoTable.rowHeight = UITableView.automaticDimension
    }
    
    func textFieldShouldReturn(_ textField: UITextField) -> Bool{
        // Add todos on return & clear text field & remvove keyboard
        if let title = textField.text, title != empty {
            addTodoMutationCloud(title: title)
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
        if ( filteredTodos[indexPath.row].isCompleted ){
            todoCell.todoCheckbox.setBackgroundImage( #imageLiteral(resourceName: "checked"), for: UIControl.State.normal)
        }else {
            todoCell.todoCheckbox.setBackgroundImage( #imageLiteral(resourceName: "unchecked"), for: UIControl.State.normal)
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
            self.removeTodosMutationCloud(indexPath: indexPath)
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
        removeAllCompletedTodosCloud()
    }
    
    // OnClick of Checkbox
    func checkBox(checked: Bool, index: Int!) {
        // Optimistic update & Change UI
        toggleTodosMutationLocal(id: filteredTodos[index].id, checked: checked)
        let index = self.todos.firstIndex{$0.id == filteredTodos[index].id}!
        todos[index].isCompleted = checked
        filteredTodos[index].isCompleted = checked
        todoTable.reloadRows(at: [IndexPath(row: index, section: 0)], with: UITableView.RowAnimation.automatic)
        
        // Update Network
        toggleTodosMutationCloud(checked: checked, index: index)
    }
    
    
    // UI Setup
    private func setupUI() {
        if(todos.count > 0){
            loadingIndicator.isHidden = true
            loadingIndicator.stopAnimating()
            todoFilter.isHidden = false
            todoTable.isHidden = false
        } else {
            loadingIndicator.isHidden = true
            loadingIndicator.stopAnimating()
            todoFilter.isHidden = true
            todoFilter.selectedSegmentIndex = 0
            todoTable.isHidden = true
            loadingTitle.text = "No Todos yet!"
        }
    }
    
    // Complete Action button toggle when on Completed Segment & there are completed items
    private func toggleCompleteAction(){
        if(todoFilter.selectedSegmentIndex == 2 && todos.filter({$0.isCompleted}).count > 0){
            todoCompleteAction.isHidden = false
        } else {
            todoCompleteAction.isHidden = true
        }
    }
    
    // Filtered Todo list
    private func getFilteredTodos(segmentIndex: Int = 0) -> [GetMyTodosQuery.Data.Todo]{
        switch segmentIndex{
        case 0:
            return self.todos
        case 1:
            return self.todos.filter { $0.isCompleted == false }
        case 2:
            return self.todos.filter { $0.isCompleted == true }
        default:
            return self.todos
        }
    }
    
    
    
    
    // Queries
    
    // Get Todo from Cloud : NOT Using - Using Watcher
    private func todoQueryCloud(){
        apollo.fetch(query: GetMyTodosQuery()){ (result, error) in
            if ((error) != nil) {
                if SessionManager.shared.logout() {
                    self.performSegue(withIdentifier: "loginVC", sender: self)
                }
                return
            }
            guard let data = result?.data else { return }
            self.todos = data.todos
            self.filteredTodos = data.todos
            DispatchQueue.main.async {
                self.setupUI()
                self.todoTable.reloadData()
            }
        }
    }
    
    // Todo Query Watcher from local cache
    private func todoQueryWatcher() -> GraphQLQueryWatcher<GetMyTodosQuery>{
        return apollo.watch(query: GetMyTodosQuery(), resultHandler: {(result, error) in
            if ((error) != nil) {
                if SessionManager.shared.logout() {
                    self.performSegue(withIdentifier: "loginVC", sender: self)
                }
                return
            }
            guard let data = result?.data else { return }
            self.todos = data.todos
            self.filteredTodos = self.getFilteredTodos(segmentIndex: self.todoFilter.selectedSegmentIndex)
                self.setupUI()
                self.todoTable.reloadData()
        })
    }
    
    // Add Todo to Cloud
    private func addTodoMutationCloud(title: String){
        apollo.perform(mutation: AddTodoMutation(todo: title, isPublic: false)) { (result, error) in
            guard let data = result?.data else { return }
            let newTodo = data.insertTodos!.returning.compactMap({GetMyTodosQuery.Data.Todo(id: $0.id, title: $0.title, createdAt: $0.createdAt, isCompleted: $0.isCompleted)})
            self.todos.insert(contentsOf: newTodo, at: 0)
            self.filteredTodos.insert(contentsOf: newTodo, at: 0)
            
            // Update local cache
            self.addTodoMutationLocal(todo: newTodo[0])
            
            // Update view
            DispatchQueue.main.async {
                let indexPaths = (0 ..< newTodo.count).map { IndexPath(row: $0, section: 0) }
                self.todoTable.beginUpdates()
                self.todoTable.insertRows(at: indexPaths , with: .automatic)
                self.todoTable.endUpdates()
            }
            
        }
    }
    
    // Add Todo to local cache
    func addTodoMutationLocal(todo: GetMyTodosQuery.Data.Todo) {
        _ = apollo.store.withinReadWriteTransaction{ transaction in
            let query = GetMyTodosQuery()
            try transaction.update(query: query) { (data: inout GetMyTodosQuery.Data) in
                data.todos.insert(todo, at: 0)
                
                _ = self.apollo.store.load(query: query).andThen({ (data) in
                    // Watch your data in local cache
//                    dump(data.data?.resultMap)
                    // Look for errors
//                    dump(data.errors)
                })
                
            }
        }
    }
    
    // Toggle Todos to Cloud
    private func toggleTodosMutationCloud(checked: Bool, index: Int!){
        apollo.perform(mutation: ToggleTodoMutation(id: filteredTodos[index].id, isCompleted: checked)) { (result, error) in
            guard let data = result?.data else { return }
            if data.updateTodos?.affectedRows == 1 { return } else {
                // With some error - revert to original state : Watcher will update the table anyways on local cache update
                self.toggleTodosMutationLocal(id: index, checked: checked)
            }
        }
    }
    
    // Toggle Todos local cache
    private func toggleTodosMutationLocal(id: Int, checked: Bool){
        _ = apollo.store.withinReadWriteTransaction{ transaction in
            let query = GetMyTodosQuery()
            try transaction.update(query: query) { (data: inout GetMyTodosQuery.Data) in
                let todos = data.todos
                guard let index = todos.firstIndex(where: {$0.id == id}) else {return}
                data.todos[index].isCompleted = checked
                
                _ = self.apollo.store.load(query: query).andThen({ (data) in
                    // Watch your data in local cache
//                    dump(data.data?.resultMap)
                    // Look for errors
//                    dump(data.errors)
                })
            }
        }
    }
    
    
    // Remove Todos from cloud
    private func removeTodosMutationCloud(indexPath: IndexPath!){
        let id = filteredTodos[indexPath.row].id
        apollo.perform(mutation: RemoveTodoMutation(id: id)) { (result, error) in
            guard let data = result?.data else { return }
            if data.deleteTodos?.affectedRows == 1 {
                let index = self.filteredTodos.firstIndex{$0.id == id}!
                self.todos.remove(at: index)
                self.filteredTodos.remove(at: index)
                // Remove from local
                self.removeTodosMutationLocal(id: id)
                DispatchQueue.main.async {
                    self.toggleCompleteAction()
                    // Delete the table view row
                    self.todoTable.deleteRows(at: [indexPath], with: .fade)
                }
            }
        }
    }
    
    // Remove all completed Todos from cloud
    private func removeAllCompletedTodosCloud(){
        apollo.perform(mutation: ClearCompletedMutation()) { (result, error) in
            guard let data = result?.data else { return }
            if data.deleteTodos?.affectedRows == self.filteredTodos.filter({$0.isCompleted}).count {
                self.todos = self.todos.filter({!$0.isCompleted})
                self.filteredTodos = self.filteredTodos.filter({!$0.isCompleted})
                self.removeTodosMutationLocal(id: -1)
                DispatchQueue.main.async {
                    self.toggleCompleteAction()
                }
            }
        }
    }
    
    // Remove Todos to local cache
    private func removeTodosMutationLocal(id: Int){
        _ = apollo.store.withinReadWriteTransaction{ transaction in
            let query = GetMyTodosQuery()
            try transaction.update(query: query) { (data: inout GetMyTodosQuery.Data) in
                let todos = data.todos
                if ( id == -1 ) {
                    data.todos = data.todos.filter({!$0.isCompleted})
                } else {
                    guard let index = todos.firstIndex(where: {$0.id == id}) else {return}
                    data.todos.remove(at: index)
                }
                
                _ = self.apollo.store.load(query: query).andThen({ (data) in
                    // Watch your data in local cache
//                    dump(data.data?.resultMap)
                    // Look for errors
//                    dump(data.errors)
                })
            }
        }
    }
}
