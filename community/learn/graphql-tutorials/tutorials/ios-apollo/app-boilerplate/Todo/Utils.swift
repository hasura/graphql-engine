import Foundation
import UIKit

class Utils {
    static let shared = Utils()
    
    func setLeftPaddingInput (ofInput: UITextField){
        ofInput.leftView = UIView(frame: CGRect(x: 0, y: 0, width: 16, height: ofInput.frame.height))
        ofInput.leftViewMode = .always
        ofInput.rightView = UIView(frame: CGRect(x: 0, y: 0, width: 16, height: ofInput.frame.height))
        ofInput.rightViewMode = .always
    }
}
