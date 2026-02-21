---
name: swiftui-accessibility
description: SwiftUI accessibility best practices. Use when writing or reviewing SwiftUI views to ensure they are accessible — correct labels, traits, tree structure, and VoiceOver behavior.
---

# SwiftUI Accessibility

SwiftUI generates an accessibility tree alongside the visual render tree. Assistive technologies (VoiceOver, Voice Control, Switch Control, Full Keyboard Access) use this tree — not the view hierarchy — to present and interact with your app. Your job is to ensure the tree is correct, concise, and navigable.

**Always test with VoiceOver.** Accessibility Inspector and audits catch structural issues, but real assistive tech reveals the actual experience.

## What's Automatic (and What Isn't)

Standard controls get full accessibility for free:

| View | Auto Label | Auto Traits/Behavior |
|------|-----------|---------------------|
| `Text` | The string | `.isStaticText` |
| `Button` | Label content text | `.isButton`, tap action |
| `Toggle` | Label text | `.isToggle`, value "on"/"off" |
| `Slider` | Label text | Adjustable, increment/decrement |
| `Picker` | Label text | Value, popup |
| `TextField` | Placeholder/label | Text field |
| `NavigationLink` | Label content | `.isButton`, `.isLink` |
| `Section` header | Header text | `.isHeader` |
| SF Symbols | Inferred from name | `.isImage` |
| `Label("Title", systemImage:)` | The title text | Varies by context (`.isButton` in a `Button`, etc.) |

**Not automatically accessible:**
- `Shape`, `Canvas`, custom-drawn content — invisible to the tree
- `Image("name")` — gets the **asset filename** as label (usually garbage)
- Layout containers (`HStack`, `VStack`, `ZStack`, `Spacer`, `Group`) — transparent, not in the tree
- Custom gesture-based controls — no traits or actions

**State-driven notifications are automatic.** Unlike UIKit, you never need `UIAccessibility.post(notification:)` for value changes. When `@State`/`@Binding` changes affect accessibility properties, SwiftUI notifies assistive tech automatically.

## The Accessibility Tree

The tree has two node types:
- **Elements** — leaf nodes VoiceOver can focus on (label, value, traits, actions)
- **Containers** — structural nodes that group children for navigation

Layout views (stacks, spacers) are transparent — their children float up. Only semantic views (text, controls, images) become elements by default.

```
Screen (root container)
├── Text "Hello"                     → element (auto)
├── VStack                            → transparent
│   ├── Text "Score"                  → element (auto)
│   └── Text "100"                    → element (auto)
├── HStack .a11yElement(.combine)     → single merged element
├── VStack .a11yElement(.contain)     → container
│   ├── Button "Edit"                 → element (child)
│   └── Button "Delete"              → element (child)
├── Image(decorative: "bg")           → not in tree
└── Button "Submit"                   → element (auto)
```

**Sort order** is geometric: top-left to bottom-right. All children within a container are navigated before moving to the next sibling. Override with `accessibilitySortPriority(_:)` (higher = first, default 0).

## Controlling Tree Structure: `.accessibilityElement(children:)`

This is the most important modifier for tree shaping. Three behaviors:

### `.ignore` (the default when called with no args)

Creates a new element, **hides all children**. The element starts blank — you must supply everything manually.

```swift
VStack {
    Text("Your score is")
    Text("1000").font(.title)
}
.accessibilityElement(children: .ignore)
.accessibilityLabel("Your score is 1000")
```

Use when children are visual fragments of one concept and `.combine` doesn't produce the right reading.

### `.combine`

Merges children's properties into a **single element**. Labels are concatenated (with VoiceOver pauses between fragments). Child buttons become custom actions. Traits are selectively merged.

```swift
HStack {
    Text(person.name)
    Spacer()
    Button("Follow") { ... }
    Button("Share") { ... }
}
.accessibilityElement(children: .combine)
// VoiceOver: "Jane Smith" with Follow and Share as custom actions
```

**This is the workhorse for list/card cells.** Without it, a `ForEach` of cells creates a flood of decontextualized "Follow, Share, Follow, Share" elements.

### `.contain`

Creates an **accessibility container**. Children remain individually focusable but are grouped — VoiceOver navigates all children before moving on. The container label is announced on entry.

```swift
VStack {
    ForEach(alerts) { alert in
        AlertCell(alert: alert)
    }
}
.accessibilityElement(children: .contain)
.accessibilityLabel("Alerts")
```

**Important:** `VStack`/`HStack` are NOT containers by default (unlike `List`). You must use `.contain` explicitly to group them. This also adds the group to VoiceOver's containers rotor.

## Labels

### Core Rules
1. **Don't repeat the trait.** Say "Play", not "Play button" — VoiceOver appends the trait.
2. **Label images at creation.** Use `Image("name", label: Text("Description"))` or add `.accessibilityLabel()`. For decorative images: `Image(decorative: "bg")` or `.accessibilityHidden(true)`.
3. **Don't trust SF Symbol names.** `slider.vertical.3` is read literally. Always verify with VoiceOver; add `.accessibilityLabel()` when the symbol name doesn't match intent.

### Label Modifiers

```swift
// Simple label
.accessibilityLabel("Play")

// Append to existing label without replacing it (iOS 18+)
.accessibilityLabel { viewProxy in
    viewProxy  // carries the view's existing auto-generated label
    Text("Unread")
}

// Conditional label — falls back to default when disabled (iOS 18+)
.accessibilityLabel("Super favorite", isEnabled: isSuperFavorite)

// Voice Control / dictation alternate names
.accessibilityInputLabels(["Send Message", "Send", "Message"])

// Pair a visual label with a control (like UIKit label-for)
.accessibilityLabeledPair(role: .label, id: pairID, in: namespace)
.accessibilityLabeledPair(role: .content, id: pairID, in: namespace)
```

### Value and Hint

```swift
.accessibilityValue("50 percent")  // Current state (read after label)
.accessibilityHint("Double tap to play")  // What happens on activation (read last, after a pause)
```

## Traits

Traits communicate the **role** of an element. SwiftUI has no `AccessibilityRole` type — roles are expressed entirely through traits.

```swift
.accessibilityAddTraits(.isButton)     // Add traits
.accessibilityRemoveTraits(.isImage)   // Remove traits
.accessibilityAddTraits([.isButton, .isHeader])  // Combine as a set
```

**All trait values:**

| Trait | Meaning |
|-------|--------|
| `.isButton` | Tappable button |
| `.isHeader` | Section header (enables heading navigation rotor) |
| `.isLink` | Navigable link |
| `.isImage` | Image content |
| `.isSearchField` | Search input |
| `.isToggle` | On/off toggle (iOS 17+) |
| `.isTabBar` | Tab bar element (iOS 18+) |
| `.isSelected` | Currently selected |
| `.isStaticText` | Non-editable text |
| `.isModal` | Modal — restricts VoiceOver to this element's contents |
| `.isSummaryElement` | Summary info shown on app launch |
| `.isKeyboardKey` | Keyboard key |
| `.allowsDirectInteraction` | Direct touch passthrough |
| `.causesPageTurn` | Auto page-turn after VoiceOver finishes |
| `.playsSound` | Plays own sound on activation |
| `.startsMediaSession` | Starts media playback |
| `.updatesFrequently` | Label/value changes often |

**Common mistake:** Adding `.onTapGesture` to an `Image` makes it tappable but VoiceOver still announces it as just an image. You must manually add `.isButton` and remove `.isImage` — or better, just use a `Button`.

## Prefer View Styles Over Custom Views

A `ButtonStyle` or `ToggleStyle` preserves all built-in accessibility even with completely custom drawing. A custom view built from shapes and gestures gets none of it.

```swift
// GOOD — all accessibility preserved
struct FancyButtonStyle: ButtonStyle {
    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .padding()
            .background(RoundedRectangle(cornerRadius: 12).fill(.blue))
    }
}

// BAD — must manually add .isButton, tap action, label...
RoundedRectangle(cornerRadius: 12)
    .fill(.blue)
    .overlay(Text("Submit"))
    .onTapGesture { ... }
```

## Making Custom Controls Accessible

### `accessibilityRepresentation` — Map to a Standard Control

Replaces the entire accessibility of a view with that of a standard control:

```swift
BudgetSlider(value: $amount)
    .accessibilityRepresentation {
        Slider(value: $amount, in: 0...500) {
            Text("Budget")
        }
    }
```

The custom slider gets adjustable trait, increment/decrement actions, and value announcements — all for free.

### `accessibilityChildren` — Synthetic Children for Drawn Content

For `Canvas`, `Shape`, or any custom-drawn view with no view hierarchy:

```swift
Canvas { context, size in
    // draws bars manually
}
.accessibilityLabel("Budget History")
.accessibilityChildren {
    HStack {
        ForEach(budgets) { budget in
            Rectangle()
                .accessibilityLabel(budget.name)
                .accessibilityValue("\(budget.amount) dollars")
        }
    }
}
```

The views in the closure are **never rendered** — they only populate the accessibility tree.

## Actions

```swift
// Named custom actions (appear in VoiceOver actions rotor)
.accessibilityAction(named: "Delete") { delete() }

// Standard action kinds: .default, .escape, .delete, .magicTap, .showMenu
.accessibilityAction(.escape) { dismiss() }

// Adjustable (VoiceOver swipe up/down)
.accessibilityAdjustableAction { direction in
    switch direction {
    case .increment: value += 1
    case .decrement: value -= 1
    @unknown default: break
    }
}

// Extract actions from a view builder (iOS 18+)
// Useful for surfacing hover/long-press content
.accessibilityActions {
    Button("Favorite") { ... }
    Button("Reply") { ... }
}
```

**Key principle:** Content behind hover, long-press, or secondary gestures should be exposed as custom actions, since navigating to dynamically-appearing content is expensive with assistive tech.

## Hiding and Identification

```swift
// Hide from accessibility entirely
.accessibilityHidden(true)

// Testing identifier (NOT spoken by VoiceOver — for XCUITest only)
.accessibilityIdentifier("submit-button")
```

## Focus

```swift
@AccessibilityFocusState var isFocused: Bool

Text("Error: invalid input")
    .accessibilityFocused($isFocused)

// Move VoiceOver focus programmatically
Button("Validate") {
    if hasError { isFocused = true }
}
```

**Caution:** Moving focus programmatically is disruptive. Only do it in response to user-initiated actions. For passive notifications, post an announcement instead.

## Announcements (iOS 17+)

```swift
AccessibilityNotification.Announcement("Item deleted").post()

// With priority — set via AttributedString
var announcement = AttributedString("Camera active")
announcement.accessibilitySpeechAnnouncementPriority = .high  // .default, .low, .high
AccessibilityNotification.Announcement(announcement).post()
```

Replaces the old `UIAccessibility.post(notification:argument:)` pattern.

## Headings

```swift
.accessibilityHeading(.h1)  // .h1 through .h6, .unspecified
```

Use for content structure. VoiceOver's heading rotor lets users jump between headings. `Section` headers get `.isHeader` automatically, but for custom layouts you set this manually.

## Less Common APIs (Reference)

Look these up when the situation arises:

| API | When to Use | Docs |
|-----|-------------|------|
| `accessibilityRotor` | Custom VoiceOver rotors for navigating specific element types (e.g., "Warnings" rotor) | [Rotors](https://developer.apple.com/documentation/swiftui/view/accessibilityrotor(_:entries:)) |
| `accessibilityCustomContent` | Structured extra info (e.g., "Rating: 4 stars") available on demand | [Custom Content](https://developer.apple.com/documentation/swiftui/view/accessibilitycustomcontent(_:_:importance:)) |
| `accessibilityChartDescriptor` | Making Swift Charts accessible with audio graphs | [Charts A11y](https://developer.apple.com/documentation/swiftui/view/accessibilitychartdescriptor(_:)) |
| `accessibilityShowsLargeContentViewer` | Long-press magnification for toolbar/tab items at large text sizes | [Large Content](https://developer.apple.com/documentation/swiftui/view/accessibilityshowslargecontentviewer()) |
| `accessibilityDirectTouch(_:options:)` | Pass touch events directly (musical instruments, drawing) | [Direct Touch](https://developer.apple.com/documentation/swiftui/view/accessibilitydirecttouch(_:options:)) |
| `accessibilityIgnoresInvertColors` | Prevent Smart Invert on photos/video | [Invert Colors](https://developer.apple.com/documentation/swiftui/view/accessibilityignoresinvertcolors(_:)) |
| `accessibilityTextContentType` | Optimize speech for source code, narrative, messaging, etc. | [Text Content](https://developer.apple.com/documentation/swiftui/view/accessibilitytextcontenttype(_:)) |
| `accessibilityDragPoint` / `accessibilityDropPoint` | Named drag/drop points for VoiceOver | [Drag & Drop](https://developer.apple.com/documentation/swiftui/view/accessibilitydragpoint(_:description:)) |
| `accessibilityQuickAction` | Apple Watch double-pinch gesture | [Quick Actions](https://developer.apple.com/documentation/swiftui/view/accessibilityquickaction(style:content:)) |
| Speech modifiers (`speechAdjustedPitch`, `speechSpellsOutCharacters`, etc.) | Fine-tune VoiceOver speech for specific content | [Speech](https://developer.apple.com/documentation/swiftui/view/speechadjustedpitch(_:)) |
| `accessibilityZoomAction` | Custom zoom gestures | [Zoom](https://developer.apple.com/documentation/swiftui/view/accessibilityzoomaction(_:)) |
| `accessibilityLinkedGroup` | Quick navigation between related elements | [Linked Groups](https://developer.apple.com/documentation/swiftui/view/accessibilitylinkedgroup(id:in:)) |

## Testing

### Accessibility Inspector
Xcode → Open Developer Tool → Accessibility Inspector. Hover over elements to inspect label, value, traits, and tree hierarchy.

### XCUITest Audits

```swift
try app.performAccessibilityAudit(for: .all) { issue in
    // return true to IGNORE the issue, false to fail the test
    return false
}
```

Audit types: `.elementDescription`, `.hitRegion`, `.contrast`, `.elementDetection`, `.clippedText`, `.traits`, `.dynamicType` (iOS), `.parentChild`, `.action` (macOS).

**Audits don't guarantee accessibility.** They catch structural issues but miss context problems (wrong labels, poor grouping, confusing navigation flow). Always test with VoiceOver.

## Quick Checklist

- [ ] Every meaningful image has a label (or is `decorative:`)
- [ ] Every interactive element has `.isButton` / appropriate trait (prefer `Button` over `.onTapGesture`)
- [ ] List/card cells use `.combine` to create single elements with contextual labels
- [ ] Section headers have `.isHeader` trait
- [ ] Custom controls use `accessibilityRepresentation` or manual traits+actions
- [ ] Hover/long-press content is surfaced as `.accessibilityActions`
- [ ] Custom-drawn content uses `accessibilityChildren` for synthetic elements
- [ ] VoiceOver reading order makes sense (test it!)
- [ ] Dynamic Type is supported (use `.font(.body)` etc., not fixed sizes)
