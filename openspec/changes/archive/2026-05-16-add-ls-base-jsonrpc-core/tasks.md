## 1. Message Model

- [x] 1.1 Add JSON-RPC id, request, response, notification, and error message models.
- [x] 1.2 Define deterministic handling for malformed envelopes and unsupported batch forms.

## 2. Codec

- [x] 2.1 Add envelope encoding for outbound requests, responses, and notifications.
- [x] 2.2 Add envelope decoding for inbound requests, responses, and notifications.
- [x] 2.3 Keep codec behavior deterministic for field ordering and error mapping.

## 3. Session Core

- [x] 3.1 Add pending-request tracking and response correlation.
- [x] 3.2 Add a transport-agnostic in-memory dispatch core.

## 4. Validation

- [x] 4.1 Add encode and decode tests for each envelope kind.
- [x] 4.2 Add request-correlation tests.
- [x] 4.3 Add malformed-message tests.
