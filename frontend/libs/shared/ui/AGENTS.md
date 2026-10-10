# AGENTS.md (shared/ui)

Nx project `shared-ui`, import path `@hasura/shared/ui`. No `src/lib` —
components live directly under `src/components/*`.

## Purpose

Design-system UI component library for the console frontend — a shared set
of styled React components (buttons, forms, dialogs, tables, toasts, alerts,
theming) built on Radix UI Themes, used across other Nx apps/libs in the
monorepo.

## Public API (`src/index.ts`)

```ts
export * from './theme'; // AppTheme, theme types
export * from './components'; // all UI components
```

## Components (folders under `src/components/`)

Alert (+ `AlertProvider`, `useHasuraAlert`,
`useDestructiveAlert`), Badge (+ `LegacyBadge`), Breadcrumbs, Button (+
`IconButton`, `InternalButtonIcon`), Card, CardedTable, Collapsible,
`console-dev-tools` (`ConsoleDevTools`), `deprecated` (`Collapse` —
explicitly legacy), Dialog, DropdownButton, DropdownMenu, **Form** (large
subtree: `base/` primitives — Checkbox, CheckboxGroup, Input,
RadioCardGroup, RadioGroup, react-select wrappers, Select, Switch, TextArea;
`control/` field wrappers — CheckboxesField, CheckboxField, CodeEditorField,
CopyableInputField, DateTimePickerField, FieldWrapper, FileInputField,
GraphQLSanitizedInputField, InputField, ListMap, RadioGroupField,
ReactSelectField, SelectField, SwitchField, TextAreaField; `SimpleForm`;
`hooks/useConsoleForm`; `dev-components/FormDebug(Window)`),
GqlCompatibilityWarning, HasuraLogo (`HasuraLogoFull`, `HasuraLogoIcon`),
IndicatorCard, KeyValuePairsSelector, LearnMoreLink, LinkBlock
(Horizontal/Vertical), Loading (`Spinner`, `SkeletonList`), MapSelector,
OpenApi3Form (dynamic form renderer for OpenAPI schemas), PaginationOffset,
RequestHeader, RequestHeadersSelector, SanitizeTips, Table, Tabs, Toasts
(`hasuraToast`, `ToastsHub`, `legacyNotifications`,
`DisplayToastErrorMessage`), Tooltip (+ `IconTooltip`), Tree (controlled
expand/check/select tree; `TreeDataNode` node type — replaced antd's `Tree`),
typography (`Text`), WarningSymbol.

Theme: `AppTheme` wraps children in Radix `<Theme accentColor="indigo">`,
imports `theme.css`, `date-input.css`, and `react-datepicker` CSS.

## Styling approach

Mixed, primarily **Tailwind CSS + Radix UI Themes**:

- `@radix-ui/themes` provides base components (`Button`, `Card`, `Theme`,
  `Spinner`, `Flex`, `Text`) that local components wrap/extend (adding
  `mode`/`disabled` variants via `clsx`).
- Tailwind utility classes used pervasively; `styles.ts` exports Tailwind
  class-string constants (`focusYellowRing`, `inputStyles`).
- Global CSS: `src/theme/theme.css` (imports Radix theme CSS + Google fonts,
  overrides CSS custom properties like `--indigo-9`), `date-input.css`.
- At least one component (`WarningSymbol`) uses a CSS Module
  (`WarningSymbol.module.scss`) — inconsistent with the rest; don't assume
  Tailwind-only when touching a component.
- `clsx` is the standard classname utility throughout (not `classnames`).

## Storybook

59 `.stories.tsx` files (no `.mdx`). This lib has no Storybook target of its
own — its stories are served by `console-legacy-ce`'s Storybook
(`libs/console/legacy-ce/.storybook/main.ts` globs `shared/ui/src/**`), so run
`yarn storybook` and its global decorators apply here too.
Notable pattern (`Button.stories.tsx`): emoji-prefixed story category names
(`⚙️ API`, `🧰 Basic`, `🎭 Variant - X`, `🔁 State - X`, `🧪 Testing - X`),
rich `docs.description` markdown blocks.

## Gotchas

- `AlertProvider` delays showing the alert via `setTimeout(..., 0)`
  specifically to avoid a Radix overlay `pointer-events: none` bug when
  transitioning from other overlay components.
- `Button` blocks `onClick` when inside a `<fieldset disabled>` as a
  workaround for a documented React bug (facebook/react#7711).
- Two badge implementations exist side by side: `Badge` and `LegacyBadge` —
  check which is current before use.
- Toasts has a legacy path too: `legacyNotifications.tsx` alongside the
  current `hasuraToast.tsx`/`ToastsHub` (built on `react-hot-toast/headless`).
- `size` prop on `Button`/`Card` overloads Radix's own `size` enum with
  custom `sm|md|lg` aliases mapped internally
  (`buttonSizes = {sm:'1', md:'2', lg:'3'}`), so raw Radix size strings also
  work.

## Testing

Vitest via Nx (`vite.config.mts`, jsdom environment, globals on). Only a
few test files exist (`Button.spec.tsx`, `Input.test.tsx`,
`InputField.test.tsx`, `MapSelector.test.tsx`, `ConsoleDevTools.spec.tsx`,
`Tree.test.tsx`) —
sparse relative to ~90 components. Uses `@testing-library/react`.
