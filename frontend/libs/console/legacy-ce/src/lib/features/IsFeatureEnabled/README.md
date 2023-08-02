# IsFeatureEnabled

React APIs to enable a feature in specific Hasura plans. Aim to replace all the sources of truth when it comes to identify if a feature is enabled or not.

## Status

### Hasura plan support status

Environments

- [x] Hasura CE (_not tested in production yet_)
- [x] Hasura EE with/without license (_not tested in production yet_)
- [ ] Hasura Cloud
- [ ] Self-hosted Hasura Cloud
- [ ] EE Classic (hybrid Hasura Cloud, with self-hosted HGE server and Hasura-hosted Lux)

Modes

- [x] Server and CLI without distinction
- [ ] Server only
- [ ] CLI only

### Dev APIs status

- [x] React hook API
- [x] React component API
- [ ] Storybook APIs
- [ ] Unit testing APIs
- [ ] Console wrapper to load all the async source of truths

### Others

- [ ] Refactor all the usages of `isProConsole` etc. APIs
- [ ] Manage also the feature flags
- [ ] Mark `window.__env` as deprecated
- [ ] Refactor all the usages of `window.__env`
- [ ] Document how to tun the Console in all the modes

## Console Usage

### Simple usage

To only render something when a feature is enabled or not:

- Using the React component API

```tsx
function Prometheus() {
  return (
    <IsFeatureEnabled feature="prometheus">
      <div>Enjoy Prometheus!</div>
    </IsFeatureEnabled>
  );
}
```

- Using the React hook API

```tsx
function Prometheus() {
  const { status } = useIsFeatureEnabled('prometheus');

  if (status === 'disabled') return null;

  return <div>Enjoy Prometheus!</div>;
}
```

### Advanced usage

To render something when a feature is enabled, or something different when the feature is disabled:

- Using the React component API

```tsx
function Prometheus() {
  return (
    <IsFeatureEnabled
      feature="prometheus"
      ifDisabled={(doNotMatch, current: { hasuraPlan }) => {
        if (doNotMatch.ee) {
          if (hasuraPlan.type === 'ce') {
            return <div>Try EE Lite and give all the paid feature a try for free!</div>;
          }

          return <div>Prometheus is enabled for EE Lite only</div>;
        }
      }}
    >
      <div>Enjoy Prometheus!</div>
    </IsFeatureEnabled>
  );
}
```

- Using the React hook API

```tsx
function Prometheus() {
  const {
    status,
    doNotMatch,
    current: { hasuraPlan },
  } = useIsFeatureEnabled('prometheus');

  if (status === 'disabled') {
    if (doNotMatch.ee) {
      if (hasuraPlan.type === 'ce') {
        return <div>Try EE Lite and give all the paid feature a try for free!</div>;
      }

      return <div>Prometheus is enabled for EE Lite only</div>;
    }
  }

  return <div>Enjoy Prometheus!</div>;
}
```

## Testing

### Storybook

❌ It's not possible to use the new APIs in Storybook yet. ❌

### Jest

❌ It's not possible to use the new APIs in Jest yet. ❌

## Adding more source of truths or Console types to the Hasura plan

No shortcuts here, look at

1. The `checkCompatibility` function and its types
2. The `checkCompatibility` runtime and types tests
3. The `store` types

## Adding new features

1. Add a new object to the `features.ts` module
2. Add is to the `features` object exposed by the `features.ts` module

## FAQ

_As a developer, I want to be sure my component is not rendered until the Hasura plan data is available because I do not want to manage the loading state._

When a Console wrapper will be ready, it will prevent rendering the Console until the source of truths, no need to manage the loading states.

_I see Rect APIs, but I do not see pure JavaScript APIs, why?_

React APIs includes "reactivity" by definition. Vanilla JavaScript APIs cannot offer the same reactivity in an easy way. If you need to consume the Hasura plan data offered by `useIsFeatureEnabled`, read it from a React component and pass it down to your vanilla JavaScript functions.

_What about getting the APIs accepting an array of features instead of just one?_

We wil evaluate if the feature is really needed because the TypeScript gymnastics needed for the `doMatch`/`doNotMatch` objects returned by the APIs are not trivial, and making them working with arrays is even worse.

_What about specifying only the required properties in the compatibility object?_

Getting all the properties explicit enforces managing them also when we add more Console types/mode and source of info. It's a by-design choice.
