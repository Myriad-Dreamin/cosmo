## 1. Lifecycle

- [ ] 1.1 Add `initialize`, `initialized`, `shutdown`, and `exit` request or notification handling.
- [ ] 1.2 Define server capability declaration and lifecycle state transitions.

## 2. Text Document Routing

- [ ] 2.1 Add typed routing for `didOpen`, `didChange`, and `didClose`.
- [ ] 2.2 Add outbound `publishDiagnostics` support.

## 3. Extensibility

- [ ] 3.1 Add typed request and notification handler registration.
- [ ] 3.2 Keep transport and process wrapper concerns out of the lifecycle layer.

## 4. Validation

- [ ] 4.1 Add lifecycle transition tests.
- [ ] 4.2 Add text-document notification routing tests.
- [ ] 4.3 Add capability shape tests for the initial server slice.
