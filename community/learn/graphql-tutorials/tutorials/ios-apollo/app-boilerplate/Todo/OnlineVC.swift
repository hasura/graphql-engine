import UIKit
import Auth0

class OnlineVC: UIViewController, UITableViewDelegate, UITableViewDataSource {
    
    @IBOutlet weak var onlineUsersTable: UITableView!
    @IBOutlet weak var onlineUserCount: UILabel!
    @IBOutlet weak var userName: UILabel!
    @IBOutlet weak var emptyView: UIStackView!
    
    // cell reuse id (cells that scroll out of view can be reused)
    let cellReuseIdentifier = "onlineUserCell"
    var profile: UserInfo!
    // Data model: These strings will be the data for the table view cells
    var onlineUsers: [String] = ["Loki", "Thor", "Dr Strange", "Hulk", "Mantis", "TChala", "Iron Man", "Thanos"]
    
    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view.
        // Register the table view cell class and its reuse id
        self.onlineUsersTable.register(UITableViewCell.self, forCellReuseIdentifier: cellReuseIdentifier)
        
        self.onlineUserCount.text = String(onlineUsers.count)
        self.userName.text = "Logged in as: \(SessionManager.shared.userName)"
        setupUI()
    }
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return self.onlineUsers.count
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        // create a new cell if needed or reuse an old one
        let cell:UITableViewCell = (self.onlineUsersTable.dequeueReusableCell(withIdentifier: cellReuseIdentifier) as UITableViewCell?)!
        
        // set the text from the data model
        cell.textLabel?.text = self.onlineUsers[indexPath.row]
        
        return cell
    }

    @IBAction func logoutAction(_ sender: Any) {
         _ = SessionManager.shared.logout()
        // Move to First Tab to bring the login screen
        self.tabBarController?.selectedIndex = 0
    }
    
    // UI utilities
    private func setupUI() {
        if (onlineUsers.count > 0) {
            onlineUsersTable.isHidden = false
            emptyView.isHidden = true
        } else {
            onlineUsersTable.isHidden = true
            emptyView.isHidden = false
        }
    }
    
    
}
