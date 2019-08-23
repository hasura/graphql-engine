import UIKit

class FeedVC: UIViewController, UITableViewDelegate, UITableViewDataSource, UITextFieldDelegate {

    @IBOutlet weak var todoInput: UITextField!
    @IBOutlet weak var todoPublicTable: UITableView!
    @IBOutlet weak var viewNotification: UIButton!
    @IBOutlet weak var notificationCount: UILabel!
    @IBOutlet weak var notifView: UIView!
    @IBOutlet weak var loadMore: UIButton!
    @IBOutlet weak var loadMoreIndicator: UIActivityIndicatorView!
    
    var publicTodos: [PublicTodo] = []
    var publicTodosNotif: [Int] = [1]
    var empty: String = ""
    let cellReuseIdentifier = "feedCell"
    
    
    override func viewWillAppear(_ animated: Bool) {
        Utils.shared.setLeftPaddingInput(ofInput: todoInput)
        toggleNotif()
        setupUI(loading: false)
        
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        // Add Dummy data
        publicTodos.append(PublicTodo(title: "This is public todo 1", username: "SomeUser1"))
        publicTodos.append(PublicTodo(title: "This is public todo 2", username: "SomeUser2"))
        publicTodos.append(PublicTodo(title: "This is public todo 3", username: "SomeUser3"))
        publicTodos.append(PublicTodo(title: "This is public todo 4", username: "SomeUser4"))
        publicTodos.append(PublicTodo(title: "This is public todo 5", username: "SomeUser5"))
        publicTodos.append(PublicTodo(title: "This is public todo 6", username: "SomeUser6"))
        publicTodos.append(PublicTodo(title: "This is public todo 7", username: "SomeUser7"))
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
        return self.publicTodos.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        var cell : UITableViewCell!
        cell = tableView.dequeueReusableCell(withIdentifier: cellReuseIdentifier)
        if cell == nil {
            cell = UITableViewCell(style: .subtitle, reuseIdentifier: cellReuseIdentifier)
        }
        cell.textLabel?.text = publicTodos[indexPath.row].username
        cell.detailTextLabel?.text = publicTodos[indexPath.row].title
        
        // Multi-line
        cell.detailTextLabel!.numberOfLines = 0
        cell.detailTextLabel!.lineBreakMode = .byWordWrapping
        
        return cell
    }
    
    // On click of view button in Notification
    @IBAction func viewNotificationAction(_ sender: Any) {
    }
    
    // On click of load more button
    @IBAction func loadMoreAction(_ sender: Any) {
    }
    
    // UI utilities
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
    
    // Toggle Notification view if we have notification
    func toggleNotif() {
        if(publicTodosNotif.count > 0){
            notifView.isHidden = false
            notificationCount.text = String(publicTodosNotif.count)
        }else {
            notifView.isHidden = true
        }
    }
    
    // Add New public todos
    func addPublicTodos(todo: PublicTodo){
    }
}

class PublicTodo {
    var title = ""
    var username = ""
    
    convenience init(title: String, username: String){
        self.init()
        self.title = title
        self.username = username
    }
}

