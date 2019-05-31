import UIKit
import Apollo

class FeedVC: UIViewController, UITableViewDelegate, UITableViewDataSource, UITextFieldDelegate {

    @IBOutlet weak var todoInput: UITextField!
    @IBOutlet weak var todoPublicTable: UITableView!
    @IBOutlet weak var viewNotification: UIButton!
    @IBOutlet weak var notificationCount: UILabel!
    @IBOutlet weak var notifView: UIView!
    @IBOutlet weak var loadMore: UIButton!
    @IBOutlet weak var loadMoreIndicator: UIActivityIndicatorView!
    
    var publicTodos: [PublicTodo] = []
    var publicTodosNotif: [Int] = []
    var empty: String = ""
    let cellReuseIdentifier = "feedCell"
    var apollo: ApolloClient!
    
    var newPublicTodos: Cancellable?
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        if( SessionManager.shared.credentials?.accessToken! != nil ) {
            self.apollo = NetworkManager.shared.apolloClient
            if self.publicTodos.isEmpty {
                getInitialPublicTodos()
            }
        }
        Utils.shared.setLeftPaddingInput(ofInput: todoInput)
        subscribeNewPublicTodos()
        setupUI(loading: false)
    }
    
    override func viewWillDisappear(_ animated: Bool) {
        super.viewWillDisappear(animated)
        unSubscribeNewPublicTodos()
    }
    
    func textFieldShouldReturn(_ textField: UITextField) -> Bool{
        // Add todos on return & clear text field & remvove keyboard
        if let title = textField.text, title != empty {
            self.addTodoToPublicCloud(title: title)
            textField.text = empty
            textField.resignFirstResponder()
        }
        return true
    }
    
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return self.publicTodos.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        var cell : UITableViewCell!
        cell = tableView.dequeueReusableCell(withIdentifier: cellReuseIdentifier)
        if cell == nil {
            cell = UITableViewCell(style: .subtitle, reuseIdentifier: cellReuseIdentifier)
        }
        cell.textLabel?.font = UIFont.boldSystemFont(ofSize: 14.0)
        cell.textLabel?.text = "@\(publicTodos[indexPath.row].username)"
        cell.detailTextLabel?.text = publicTodos[indexPath.row].title
        
        // Multi-line
        cell.detailTextLabel!.numberOfLines = 0
        cell.detailTextLabel!.lineBreakMode = .byWordWrapping
        
        return cell
    }
    
    // On click of view button in Notification
    @IBAction func viewNotificationAction(_ sender: Any) {
        self.getNotifTodos(id: publicTodosNotif[0])
        self.publicTodosNotif.removeAll()
        toggleNotifView()
    }
    
    // On click of load more button
    @IBAction func loadMoreAction(_ sender: Any) {
        self.setupUI(loading: true)
        getOldPublicTodos(id: self.publicTodos.last!.id)
    }
    
    
    // UI Utilities
    private func setupUI(loading: Bool){
        if (publicTodos.isEmpty){
            todoPublicTable.isHidden = true
        }else {
            todoPublicTable.isHidden = false
            if loading {
                loadMore.isHidden = true
                self.loadMoreIndicator.isHidden = false
            }else {
                loadMore.isHidden = false
                self.loadMoreIndicator.isHidden = true
            }
        }
    }
    
    private func toggleNotif(id: [Int]) {
        self.publicTodosNotif.append(id[0])
        toggleNotifView()
    }
    
    private func toggleNotifView(){
        if(self.publicTodosNotif.count > 0){
            notifView.isHidden = false
            notificationCount.text = String(self.publicTodosNotif.count)
        }else {
            self.publicTodosNotif.removeAll()
            notifView.isHidden = true
        }
    }
    
    // Add Public todos to local
    private func addPublicTodoLocal(todos: [PublicTodo]){
        self.publicTodos.insert(contentsOf: todos, at: 0)
        let indexPaths = (0 ..< todos.count).map { IndexPath(row: $0, section: 0) }
        DispatchQueue.main.async {
            self.todoPublicTable.beginUpdates()
            self.todoPublicTable.insertRows(at: indexPaths, with: .automatic)
            self.todoPublicTable.endUpdates()
        }
        self.setupUI(loading: false)
    }
    
    
    // Queries
    
    // Get Public Todos from cloud
    private func getInitialPublicTodos(){
        apollo.fetch(query: GetInitialPublicTodosQuery()) { (result, err) in
            guard let data = result?.data else { return }
            let newTodos = data.todos.compactMap { PublicTodo(title: $0.title, username: $0.user.name, id: $0.id) }
            self.publicTodos = newTodos
            DispatchQueue.main.async {
                self.todoPublicTable.reloadData()
                self.setupUI(loading: false)
            }
        }
    }
    
    // Get Subscribed Todos from notification
    private func getNotifTodos(id: Int){
        apollo.fetch(query: GetNewPublicTodosQuery(latestVisibleId: id)) { (result, err) in
            guard let data = result?.data else { return }
            // Show notification
            let todos = data.todos.compactMap{ PublicTodo(title: $0.title, username: $0.user.name, id: $0.id)}
            self.addPublicTodoLocal(todos: todos)
        }
    }
    
    // Get Old Todos
    private func getOldPublicTodos(id: Int){
        apollo.fetch(query: GetOldPublicTodosQuery(oldestTodoId: id)) { (result, err) in
            guard let data = result?.data else { return }
            let newTodos = data.todos.compactMap { PublicTodo(title: $0.title, username: $0.user.name, id: $0.id) }
            self.publicTodos += newTodos
            DispatchQueue.main.async {
                self.todoPublicTable.reloadData()
                self.setupUI(loading: false)
            }
        }
    }
    
    // Add Todos to cloud
    private func addTodoToPublicCloud(title: String){
        apollo.perform(mutation: AddTodoMutation(todo: title, isPublic: true)) { (result, error) in
            if((error) != nil) { dump(error); return}
        }
    }
    
    // Subscribe to New Public Todos
    private func subscribeNewPublicTodos() {
        newPublicTodos = apollo.subscribe(subscription: NotifyNewPublicTodosSubscription()) { (result, error) in
            if((error) != nil) { dump(error); return }
            guard let data = result?.data else { return }
            if (self.publicTodos.isEmpty || self.publicTodos.filter({ el in el.id == data.todos[0].id }).count > 0) {
                return
            }
            else {
                DispatchQueue.main.async {
                    self.toggleNotif(id: data.todos.compactMap{$0.id - 1})
                }
            }
            
        }
    }
    
    // Un-subscribe to New Public Todos
    private func unSubscribeNewPublicTodos() {
        newPublicTodos?.cancel()
    }
    
}

class PublicTodo {
    var id = 0
    var title = ""
    var username = ""
    
    convenience init(title: String, username: String, id: Int){
        self.init()
        self.id = id
        self.title = title
        self.username = username
    }
}



