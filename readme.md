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
portal _modal unit Modal.component modalInput Nothing (Just <<< HandleModal)
```

The component within the portal can be used exactly as if it were just a regular child component -- you can send queries, subscribe to outputs, and use the component types as before.

### Limitations

Due to the use of `runUI`, only components which can be run directly in `Aff` are possible to send through a portal. You won't be able to use your custom application monad.
