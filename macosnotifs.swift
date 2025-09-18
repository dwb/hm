import AppKit
import ApplicationServices

// Look for an action name & action filter supplied on the command line.
let arguments = CommandLine.arguments.dropFirst()
var actionName = arguments.first?.lowercased()
switch actionName {  // Handle defective action specifiers. The first string specified is Apple-defined.
case "axshowmenu", "showmenu", "menu":
  actionName = "AXShowMenu"
case "axpress", "press":
  actionName = "AXPress"
case "name:show details\ntarget:0x0\nselector:(null)", "show details", "showdetails", "details":
  actionName = "Name:Show Details\nTarget:0x0\nSelector:(null)"
case "name:show\ntarget:0x0\nselector:(null)", "show":
  actionName = "Name:Show\nTarget:0x0\nSelector:(null)"
case "name:close\ntarget:0x0\nselector:(null)", "close":
  actionName = "Name:Close\nTarget:0x0\nSelector:(null)"
case "":
  actionName = nil
case nil:
  break
case "-h", "--help":
  exit(
    0,
    withErrorMessage:
      "usage: NotificationParser <performActionName> <performActionFilter>\n\nReturn Format:\nIndex (1-Based), Bundle ID, Title[, Subtitle], Message"
  )
default:
  exit(4, withErrorMessage: "Unrecognized action specified")
}
var actionFilter = arguments.dropFirst().first
if actionFilter == "" { actionFilter = nil }

// Get the accessibility object for the Notification Center.
let notificationCenter = NSRunningApplication.runningApplications(
  withBundleIdentifier: "com.apple.notificationcenterui")
guard let pid = notificationCenter.first?.processIdentifier else {
  exit(2, withErrorMessage: "Unable to get process ID of Notification Center")
}
let appRef = AXUIElementCreateApplication(pid)
let app = UIElement(appRef)
guard let app else {
  exit(
    5,
    withErrorMessage:
      "Unable to access the Accessibility hierarchy (check the permission for the parent application in System Settings > Privacy & Security > Accessibility)"
  )
}

// Get the main Notification Center window.
let windowRefs = getAttributeValue(element: appRef, attributeName: "AXWindows") as? [AXUIElement]  // kAXWindowsAttribute
if windowRefs?.count == 0 {
  exit(0)
}
guard windowRefs?.count == 1 else {
  exit(3, withErrorMessage: "Unable to identify main Notification Center window")
}
let windowRef = windowRefs![0]
let window = UIElement(windowRef)!

// Get the scroll area.
let scrollArea = window.child(1)?.child(1)?.child(1)
guard let scrollArea else {
  exit(3, withErrorMessage: "Unable to get Notification Center scroll area")
}

// Get the button objects, containing the notifications' descriptions & actions.
var notifications = scrollArea.children
guard notifications.count > 1 else {
  exit(3, withErrorMessage: "Unable to get notifications (button elements)")
}
notifications.removeLast()  // Menu Button

// Print the descriptions.
for (i, var notification) in notifications.enumerated() {
  guard let notificationText = notification.accessibilityDescription else {
    exit(
      3, withErrorMessage: "Unable to get AXDescription attribute of notification (button element)")
  }
  // Replace any linefeeds in the title/subtitle/message with a vertical tab.
  print(
    "\(i + 1), \(notification.bundleID ?? "unknown"), \(notificationText.replacingOccurrences(of: "\n", with:"\u{0B}"))"
  )
}

// Perform the action specified on the command line.
let filteredNotifications = notifications.filter {
  actionFilter == nil || $0.accessibilityDescription == actionFilter
}
for notification in filteredNotifications.reversed() {
  notification.performAction(actionName)
}

// Exit.
exit(0)

// WRAPPER TYPE.
struct UIElement: CustomStringConvertible {
  let reference: AXUIElement
  let role: String
  let title: String?
  let accessibilityDescription: String?

  lazy var bundleID: String? = {  // Use only for notification (button element)
    let bid = stringValueForAttribute("AXStackingIdentifier")
    guard let bid else { return nil }
    return String(bid[bid.index(bid.startIndex, offsetBy: 17)...])
  }()

  init?(_ reference: CFTypeRef?) {
    let element = reference as! AXUIElement
    self.reference = element
    let role = getAttributeValue(element: element, attributeName: "AXRole") as? String  // kAXRoleAttribute
    guard let role else { return nil }  // AXRole is required for all Accessibility objects
    self.role = role
    self.title = getAttributeValue(element: element, attributeName: "AXTitle") as? String  // kAXTitleAttribute
    self.accessibilityDescription =
      getAttributeValue(element: element, attributeName: "AXDescription") as? String  // kAXDescriptionAttribute
  }

  var description: String {  // For CustomStringConvertible, not AXDescription
    let name = title == nil ? "(Unnamed)" : "\"\(title!)\""
    return "\(role) \(name)"
  }

  func child(_ index: Int) -> UIElement? {  // 1-based, negative indexing supported.
    let childrenRefs =
      getAttributeValue(element: reference, attributeName: "AXChildren") as? [AXUIElement]  // kAXChildrenAttribute
    guard let childrenRefs else { return nil }
    guard index != 0 else { fatalError("UIElement.child is 1-based; not valid for an index of 0") }

    let i = index < 0 ? childrenRefs.count + index + 1 : index - 1  // Convert to 0-based, positive indexing.
    guard i < childrenRefs.count && i > -1 else { return nil }
    return UIElement(childrenRefs[i])
  }

  var children: [UIElement] {
    let childrenRefs =
      getAttributeValue(element: reference, attributeName: "AXChildren") as? [AXUIElement]  // kAXChildrenAttribute
    guard let childrenRefs else { return [] }

    var children: [UIElement] = []
    for childRef in childrenRefs {
      children.append(UIElement(childRef)!)
    }
    return children
  }

  func performAction(_ actionName: String?) {
    if let actionName { AXUIElementPerformAction(reference, actionName as NSString) }
  }

  var attributeNames: [String] {
    var names: CFArray?
    let err = AXUIElementCopyAttributeNames(reference, &names)
    guard err == .success else { return [] }
    return names as! [String]
  }

  func stringValueForAttribute(_ attributeName: String) -> String? {
    return getAttributeValue(element: reference, attributeName: attributeName) as? String
  }
}

// HELPER FUNCTIONS.
// Exit with error message.
func exit(_ code: CInt, withErrorMessage message: String) -> Never {
  fputs(message, stderr)
  exit(code)
}

// Get specified attribute.
func getAttributeValue(element: AXUIElement, attributeName: String) -> CFTypeRef? {
  var value: CFTypeRef?
  let err = AXUIElementCopyAttributeValue(element, attributeName as NSString, &value)
  guard err == .success else { return nil }
  return value
}
