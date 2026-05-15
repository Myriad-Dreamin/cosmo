## 1. Message Model

- [ ] 1.1 Add JSON-RPC id, request, response, notification, and error message models.
- [ ] 1.2 Define deterministic handling for malformed envelopes and unsupported batch forms.

## 2. Codec

- [ ] 2.1 Add envelope encoding for outbound requests, responses, and notifications.
- [ ] 2.2 Add envelope decoding for inbound requests, responses, and notifications.
- [ ] 2.3 Keep codec behavior deterministic for field ordering and error mapping.

## 3. Session Core

- [ ] 3.1 Add pending-request tracking and response correlation.
- [ ] 3.2 Add a transport-agnostic in-memory dispatch core.

## 4. Validation

- [ ] 4.1 Add encode and decode tests for each envelope kind.
- [ ] 4.2 Add request-correlation tests.
- [ ] 4.3 Add malformed-message tests.
