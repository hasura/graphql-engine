//
//  LoginVC.swift
//  Todo
//
//  Created by Manish Kumar on 03/05/19.
//  Copyright © 2019 Manish Kumar. All rights reserved.
//

import UIKit
import Auth0
import SimpleKeychain

class LoginVC: UIViewController {

    @IBOutlet weak var loadingIndicator: UIActivityIndicatorView!
    @IBOutlet weak var loginButton: UIButton!
    
    @IBAction func loginAction(_ sender: Any) {
        loginButton.isHidden = true
        loadingIndicator.isHidden = false
        loadingIndicator.startAnimating()
        
        SessionManager.shared.patchMode = false
        self.checkToken() {
            self.showLoginWithPatch()
        }
    }
    
    fileprivate func showLoginWithPatch() {
        guard let clientInfo = plistValues(bundle: Bundle.main) else { return }
        SessionManager.shared.patchMode = true
        Auth0
            .webAuth()
            .scope("openid profile offline_access read:current_user update:current_user_metadata")
            .audience("https://" + clientInfo.domain + "/api/v2/")
            .start {
                switch $0 {
                case .failure(let error):
                    self.loginButton.isHidden = false
                    self.loadingIndicator.isHidden = true
                    print("Error: \(error)")
                case .success(let credentials):
                    if(!SessionManager.shared.store(credentials: credentials)) {
                        print("Failed to store credentials")
                    } else {
                        SessionManager.shared.retrieveProfile { error in
                            DispatchQueue.main.async {
                                guard error == nil else {
                                    print("Failed to retrieve profile: \(String(describing: error))")
                                    return self.showLoginWithPatch()
                                }
                                NetworkManager.shared.setApolloClient(accessToken: credentials.idToken!)
                                self.dismiss(animated: true, completion: nil)
                            }
                        }
                    }
                }
            }
    }
    
    fileprivate func checkToken(callback: @escaping () -> Void) {
        SessionManager.shared.renewAuth { error in
            DispatchQueue.main.async {
                    SessionManager.shared.retrieveProfile { error in
                        DispatchQueue.main.async {
                            guard error == nil else {
                                print("Failed to retrieve profile: \(String(describing: error))")
                                return callback()
                            }
                            NetworkManager.shared.setApolloClient(accessToken: (SessionManager.shared.credentials?.idToken!)!)
                            self.dismiss(animated: true, completion: nil)
                        }
                    }
                }
            }
        }
}
