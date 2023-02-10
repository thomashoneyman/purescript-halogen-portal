# Halogen Portal

When you render an element or component in Halogen, it's inserted within the nearest parent node. Sometimes it's useful to insert a child into a different location in the DOM, especially for:

- modals
- dialogs
- tooltips
- loading bars
- breaking elements out of parents with `overflow: hidden` or `z-index` set

This component allows you to take any child component and mount it at a target node in the DOM instead of directly within its parent. All you need to do is use the `portal` function instead of the traditional `slot` function:

```purs
-- old:
HH.slot _modal unit Modal.component modalInput (Just <<< HandleModal)

-- new: this mounts to the `<body>` node instead
portalAff _modal unit Modal.component modalInput Nothing (Just <<< HandleModal)
```

The component within the portal can be used exactly as if it were just a regular child component -- you can send queries, subscribe to outputs, and use the component types as before.

### Testing locally

Build the example app:

```sh
spago bundle-app --path 'example/**/*.purs' --to dist/app.js --config spago.example.dhall
```

Open `dist/index.html` in your browser to explore the examples.

### Limitations

Due to the use of `runUI`, only components which can be easily interpreted into `Aff` can be used. This includes `Aff` and `ReaderT r Aff`, and that's about it. More specifically, you can provide a function `m (n ~> Aff)` that can pull in the monadic context of the parent to interpret the child component into `Aff`, but effects from the child component will not bubble up to the parent component at all.
