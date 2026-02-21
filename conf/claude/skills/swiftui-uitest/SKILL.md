---
name: swiftui-ui-testing
description: SwiftUI UI testing with XCUITest. Use when writing, reviewing, or debugging UI tests for SwiftUI apps. Covers element queries, the accessibility tree mapping, waiting patterns, and anti-flakiness practices.
---

# SwiftUI UI Testing (XCUITest)

**Prerequisite:** Read the `swiftui-accessibility` skill first. XCUITest operates on the accessibility tree — not the view hierarchy. Every concept there (elements, containers, `.combine`, `.contain`, `.ignore`, traits, labels, identifiers) directly determines what your tests can see and interact with. If the accessibility tree is wrong, the tests will be fragile or impossible to write well.

## Principles

1. **Every test must assert something valuable.** A test that taps a button and checks `exists` on something that was already there is worse than no test — it costs CI time, creates false confidence, and will eventually break for an unrelated reason.
2. **Every test must be honest.** If a test can pass when the feature is broken, it's dishonest. If it fails when the feature works, it's flaky. Both are worse than no test.
3. **Find elements by identity, not by position.** Index-based access (`element(boundBy: 2)`) breaks when any unrelated UI changes. Displayed text changes with localization. Use `accessibilityIdentifier` for stability.
4. **Wait for state, don't sleep.** Use `waitForExistence` and property waits. Never `sleep()`. Never bare `.exists` for async content.

## The Test–Accessibility Bridge

XCUITest queries resolve against the accessibility tree. The mapping:

| SwiftUI | XCUITest Query | Notes |
|---------|---------------|-------|
| `Text("x")` | `staticTexts["x"]` | Matched via `label` |
| `Button("x") {}` | `buttons["x"]` | Matched via `label` |
| `NavigationLink` | `buttons` | It's a button in the a11y tree |
| `Toggle` | `switches` or `toggles` | Historically `.switch`; `toggles` query added in Xcode 16.3 |
| `TextField` | `textFields` | |
| `SecureField` | `secureTextFields` | |
| `Slider` | `sliders` | |
| `Stepper` | `steppers` | |
| `Picker` | `pickers` | |
| `List` | `tables` / `cells` | Backed by UICollectionView |
| `Image` | `images` | |
| `NavigationStack` title | `navigationBars["Title"]` | |
| `TabView` tabs | `tabBars.buttons["Tab"]` | |
| `.alert()` | `alerts` | |
| `.sheet()` | `sheets` | |
| `HStack`/`VStack`/`ZStack` | `otherElements` or transparent | See a11y skill on tree structure |

### How Subscript Matching Works

`app.buttons["x"]` matches `"x"` against **any** of these element properties:
- `identifier` — from `.accessibilityIdentifier("x")`
- `label` — from `.accessibilityLabel("x")` or auto-derived text
- `title`
- `value` — from `.accessibilityValue("x")`
- `placeholderValue`

A match on **any one** is sufficient. This means an element with identifier `"Save"` and a completely different element with label `"Save"` both match `["Save"]` — causing ambiguity. Prefer `accessibilityIdentifier` for test-facing identity, and reserve `accessibilityLabel` for the user-facing description.

### How Accessibility Modifiers Reshape the Test Tree

| Modifier | Effect on XCUITest |
|----------|-------------------|
| `.accessibilityIdentifier("id")` | Sets `identifier` — the stable query target |
| `.accessibilityLabel("label")` | Sets `label` — matchable but localization-sensitive |
| `.accessibilityHidden(true)` | **Element disappears** — queries can't find it |
| `.accessibilityElement(children: .combine)` | Children vanish, merged into one element |
| `.accessibilityElement(children: .contain)` | Children remain, grouped under a container |
| `.accessibilityElement(children: .ignore)` | Children vanish, parent is blank |
| `.accessibilityAddTraits(.isButton)` | May change element type classification |

## Finding Elements

### By Identifier (Best)

```swift
// In the view:
Button("Submit Order") { ... }
    .accessibilityIdentifier("submit-order")

// In the test:
app.buttons["submit-order"].tap()
```

Identifiers are not localized, not spoken by VoiceOver, and don't change when you tweak copy. They are the only stable query target.

### By Predicate (When Needed)

```swift
// Match on multiple criteria
let pred = NSPredicate(format: "isEnabled == true AND identifier BEGINSWITH 'item-'")
let items = app.buttons.matching(pred)

// Useful key paths: identifier, label, title, value, placeholderValue,
//                   isEnabled, isSelected, elementType
// String operators: ==, CONTAINS[c], BEGINSWITH, ENDSWITH, LIKE, MATCHES
```

### By Containment (Finding Parents via Children)

```swift
// Find the cell that contains a specific text
app.cells.containing(.staticText, identifier: "Jane Smith").element
```

This is powerful for list rows where `.combine` merges children. The cell is one element, but `containing` still matches against text that was merged into it.

### Query Methods

| Method | Returns | Use |
|--------|---------|-----|
| `query["id"]` | `XCUIElement` | Most common — subscript match |
| `query.firstMatch` | `XCUIElement` | Faster — stops at first hit |
| `query.element` | `XCUIElement` | **Fails if 0 or 2+ matches** |
| `query.element(boundBy: n)` | `XCUIElement` | **Fragile** — avoid |
| `query.count` | `Int` | Forces full evaluation |
| `query.matching(pred)` | `XCUIElementQuery` | Narrows results |
| `query.containing(type, id)` | `XCUIElementQuery` | Filters by descendant |
| `query.children(matching:)` | `XCUIElementQuery` | Direct children only |
| `query.descendants(matching:)` | `XCUIElementQuery` | All descendants |

## Waiting

```swift
// Wait for existence (the bread and butter)
XCTAssertTrue(element.waitForExistence(timeout: 5))

// Wait for disappearance
XCTAssertTrue(element.waitForNonExistence(timeout: 5))

// Wait for a property value
element.wait(for: \.isEnabled, toEqual: true, timeout: 5)
```

**Never use `sleep()`.** It's either too short (flaky) or too long (slow). `waitForExistence` returns immediately when the element appears.

**`exists` vs `waitForExistence`:** `.exists` is a point-in-time check. For anything that appears after a state change or network call, use `waitForExistence`.

**`exists` vs `isHittable`:** An element can `exist` in the tree but be offscreen or obscured. If you need to verify visibility, check `isHittable` too. Note: `tap()` auto-scrolls offscreen elements in lazy containers, but `exists` does not trigger scrolling.

## Interactions

```swift
element.tap()                               // Tap
element.doubleTap()                         // Double tap
element.press(forDuration: 1.5)             // Long press
element.swipeUp()                           // Swipe (also: Down, Left, Right)
element.swipeUp(velocity: .fast)            // With velocity

// Text input — element MUST have keyboard focus
field.tap()                                  // Focus first!
field.typeText("hello@example.com")

// Sliders
slider.adjust(toNormalizedSliderPosition: 0.75)

// Picker wheels
picker.pickerWheels.element.adjust(toPickerWheelValue: "March")
```

## Test Setup

```swift
class MyFeatureUITests: XCTestCase {
    let app = XCUIApplication()

    override func setUpWithError() throws {
        continueAfterFailure = false  // Stop on first failure — prevents cascading nonsense
        app.launchArguments = ["--ui-testing", "--disable-animations"]
        app.launch()
    }
}
```

**Disable animations** for test reliability. In your app:

```swift
#if DEBUG
if CommandLine.arguments.contains("--disable-animations") {
    UIView.setAnimationsEnabled(false)
}
#endif
```

**Use launch arguments** to configure test state (mock backends, seed data, skip onboarding). The app reads `CommandLine.arguments` or `ProcessInfo.processInfo.environment`.

## A Good Test

This test is honest, valuable, and stable:

```swift
func testAddingItemAppearsInListAndPersistsCount() throws {
    // ARRANGE: verify starting state
    let list = app.tables["grocery-list"]
    XCTAssertTrue(list.waitForExistence(timeout: 3))
    let initialCount = list.cells.count

    // ACT: add an item through the real UI flow
    app.buttons["add-item"].tap()

    let nameField = app.textFields["item-name-field"]
    XCTAssertTrue(nameField.waitForExistence(timeout: 3),
                  "Add-item form did not appear")
    nameField.tap()
    nameField.typeText("Oat milk")
    app.buttons["save-item"].tap()

    // ASSERT: the item actually appeared and the list grew
    let newItem = list.cells.containing(.staticText, identifier: "Oat milk").element
    XCTAssertTrue(newItem.waitForExistence(timeout: 3),
                  "New item did not appear in the list")
    XCTAssertEqual(list.cells.count, initialCount + 1,
                   "List count should increase by exactly one")
}
```

**Why this is good:**

- **Valuable assertion**: It checks that a *specific* item appeared (not just "something exists"), and that the list count increased by exactly one. If the feature breaks, this test fails.
- **Stable identifiers**: `"grocery-list"`, `"add-item"`, `"item-name-field"`, `"save-item"` are all `accessibilityIdentifier` values — not display text, not indexes.
- **Proper waits**: `waitForExistence` after every state transition (sheet appearing, item saving). No `sleep()`.
- **The "Oat milk" text match is intentional**: It's the exact content we just typed, so matching on it validates the data flow, not just the UI structure. This is the thing under test.
- **Meaningful failure messages**: Each assertion explains what went wrong.
- **Clean a11y tree**: The test relies on the view having proper `accessibilityIdentifier` on the list, buttons, and fields, and `.combine` (or `.contain`) on list cells so that `containing(.staticText, identifier: "Oat milk")` works.

The corresponding view code:

```swift
List(items) { item in
    HStack {
        Text(item.name)
        Spacer()
        if item.isPurchased {
            Image(systemName: "checkmark")
                .accessibilityLabel("Purchased")
        }
    }
    .accessibilityElement(children: .combine)
}
.accessibilityIdentifier("grocery-list")
```

## A Bad Test

```swift
func testAddItem() throws {
    app.buttons.element(boundBy: 0).tap()               // ❗ What button? Breaks if anything is reordered
    sleep(2)                                             // ❗ Arbitrary wait
    app.textFields.element.typeText("Milk")              // ❗ Ambiguous if >1 text field. No tap() for focus
    app.buttons["Save"].tap()                            // ❗ Matches on label — breaks under localization
    XCTAssertTrue(app.staticTexts["Milk"].exists)        // ❗ Point-in-time check on async content
                                                         // ❗ Only checks existence, not WHERE it appeared
                                                         //   (could be in a toast, an error message, anywhere)
}
```

**Why this is bad:**

- **`element(boundBy: 0)`** — no one reading the test knows what this button is. Adding a toolbar button somewhere else shifts the index and silently breaks the test (or worse, makes it tap the wrong thing and still pass).
- **`sleep(2)`** — too slow on fast machines, too short on slow CI runners. Wastes 2 seconds every run regardless.
- **`.element` on `textFields`** — crashes at runtime if there are two text fields. And it doesn't tap first, so `typeText` will raise an error about keyboard focus.
- **`buttons["Save"]`** — matches on the display label. Breaks in French ("Enregistrer"), German ("Speichern"), etc. Use an identifier.
- **`staticTexts["Milk"].exists`** — this is a bare `exists` check (no wait) on content that appears asynchronously after a save operation. It's a race condition. And even if it passes, it only proves *some* text "Milk" exists *somewhere* in the app — not that it's in the list, not that the list count changed, not that the data was persisted.
- **No meaningful assertion** — this test could pass if the text field still shows "Milk" after a failed save. It asserts almost nothing.

## SwiftUI-Specific Pitfalls

### Lazy Loading
`List`, `LazyVStack`, etc. only materialize visible rows. Offscreen elements don't exist in the tree. `tap()` auto-scrolls, but `exists` does not. For existence checks on items that might be offscreen, scroll first:

```swift
func scrollToFind(_ element: XCUIElement, in scrollable: XCUIElement, maxSwipes: Int = 10) -> Bool {
    for _ in 0..<maxSwipes {
        if element.waitForExistence(timeout: 0.5) { return true }
        scrollable.swipeUp()
    }
    return false
}
```

### Combined Elements
When a cell uses `.accessibilityElement(children: .combine)`, the child `Text` and `Button` views are no longer individually queryable. The cell becomes one element. Use `containing(.staticText, identifier:)` on the cell query to find it by its merged content, or give the cell an `accessibilityIdentifier`.

### Navigation and Sheets
After navigation or sheet presentation, new content appears asynchronously. Always `waitForExistence` on the first element of the new screen before interacting.

## Reading Element State

Use `element.value` to read back control state — essential for asserting what a control *contains*, not just that it exists:

```swift
// Toggle state ("1" for on, "0" for off on iOS)
let toggle = app.switches["notifications-toggle"]
XCTAssertEqual(toggle.value as? String, "1", "Toggle should be on")

// Text field content
let field = app.textFields["name-field"]
XCTAssertEqual(field.value as? String, "Jane")

// Slider position (normalized 0–1 string like "50%")
let slider = app.sliders["volume-slider"]

// The label property gives you what VoiceOver reads
XCTAssertEqual(app.staticTexts["status-label"].label, "3 items remaining")
```

`value` is typed as `Any?` — cast to `String` for most controls. `label` is always a `String`.

## Debugging

```swift
print(app.debugDescription)  // Dumps the ENTIRE accessibility tree as XCUITest sees it
```

This is the single most useful debugging tool. When a query isn't matching, dump the tree and look at what's actually there. The output shows element types, identifiers, labels, values, frames, and the full hierarchy.

For visual inspection, use **Accessibility Inspector** (Xcode → Open Developer Tool → Accessibility Inspector) — hover over elements in the running app to see their type, identifier, label, value, and traits in real time.

## Accessibility Audits in Tests

```swift
func testAccessibility() throws {
    // Navigate to the screen under test first
    app.tabBars.buttons["settings-tab"].tap()

    try app.performAccessibilityAudit(for: [
        .elementDescription, .hitRegion, .contrast, .dynamicType
    ]) { issue in
        // Return true to IGNORE the issue, false to fail
        return false
    }
}
```

Audit types: `.elementDescription`, `.hitRegion`, `.contrast`, `.elementDetection`, `.clippedText`, `.traits`, `.dynamicType` (iOS), `.parentChild`, `.action` (macOS).

These catch structural problems (missing labels, tiny tap targets, low contrast) but don't replace testing with real assistive technology.
