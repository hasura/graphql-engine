import UIKit
import Auth0
import Apollo
import Foundation

class OnlineVC: UIViewController, UITableViewDelegate, UITableViewDataSource {
    
    @IBOutlet weak var onlineUsersTable: UITableView!
    @IBOutlet weak var onlineUserCount: UILabel!
    @IBOutlet weak var userName: UILabel!
    @IBOutlet weak var emptyView: UIStackView!
    
    var apollo: ApolloClient!
    var onlineUsersSub: Cancellable?
    
    var onlineUsers: [String] = []
    var registerSelfAsOnlineTimer: Timer?
    // cell reuse id (cells that scroll out of view can be reused)
    let cellReuseIdentifier = "onlineUserCell"
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        if(SessionManager.shared.credentials?.accessToken! != nil) {
            self.apollo = NetworkManager.shared.apolloClient
        }
        subscribeOnlineUsers()
    }
    
    override func viewWillDisappear(_ animated: Bool) {
        super.viewWillDisappear(animated)
        unSubscribeOnlineUsers()
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view.
        // Register the table view cell class and its reuse id
        self.onlineUsersTable.register(UITableViewCell.self, forCellReuseIdentifier: cellReuseIdentifier)
        
        self.onlineUserCount.text = String(onlineUsers.count)
        self.userName.text = "Logged in as: \(SessionManager.shared.userName)"
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
    
    
    // Subscribe for Online users
    private func subscribeOnlineUsers(){
        onlineUsersSub = apollo.subscribe(subscription: GetOnlineUsersSubscription()) { (result, error) in
            if((error) != nil) { dump(error); return }
            guard let data = result?.data else { return }
            self.onlineUsers = data.onlineUsers.compactMap{$0.user?.name}
            DispatchQueue.main.async {
                self.onlineUserCount.text = String(self.onlineUsers.count)
                self.onlineUsersTable.reloadData()
                self.setupUI()
            }
        }
        registerSelfAsOnlineTimer = Timer.scheduledTimer(timeInterval: 30, target: self, selector: #selector(updateLastSeenMutationCloud), userInfo: nil, repeats: true)
        registerSelfAsOnlineTimer?.fire()
    }
    
    
    // Un-subscribe for online users
    private func unSubscribeOnlineUsers(){
        onlineUsersSub?.cancel()
        registerSelfAsOnlineTimer?.invalidate()
    }
    
    
    // Update Last Seen Mutation cloud
    @objc func updateLastSeenMutationCloud() {
        let date = Date()
        let formatter = DateFormatter()
        formatter.calendar = Calendar(identifier: .iso8601)
        formatter.locale = Locale(identifier: "en_US_POSIX")
        formatter.timeZone = TimeZone(secondsFromGMT: 0)
        formatter.dateFormat = "yyyy-MM-dd'T'HH:mm:ss.SSSXXXXX"
        print(formatter.string(from: date))
        let currentTime = String(formatter.string(from: date))
        apollo.perform(mutation: UpdateLastSeenMutation(now: currentTime)) { (result, error) in
            if((error) != nil) { dump(error); return }
            guard let data = result?.data else { return }
            dump(data)
        }
    }
    
}
