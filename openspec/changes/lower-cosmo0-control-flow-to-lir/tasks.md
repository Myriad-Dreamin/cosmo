## 1. Block Builder

- [ ] 1.1 Add a LIR block builder for creating labels, appending operations, and sealing terminators.
- [ ] 1.2 Track loop break and continue targets during lowering.
- [ ] 1.3 Preserve value-producing expression results across structured control-flow joins where supported.

## 2. Structured Control Flow

- [ ] 2.1 Lower `if` and `else` expressions into conditional branches and join blocks.
- [ ] 2.2 Lower `while` and `loop` into explicit header, body, continue, and exit blocks.
- [ ] 2.3 Lower `break` and `continue` into branches to the active loop targets.
- [ ] 2.4 Lower restricted `for` loops using typed iterable descriptor information.

## 3. Variants And Match

- [ ] 3.1 Lower variant construction into explicit tag and payload LIR representation.
- [ ] 3.2 Lower variant tag checks and payload extraction operations.
- [ ] 3.3 Lower match expressions into branch chains or dispatch blocks with wildcard fallback support.
- [ ] 3.4 Preserve branch result type consistency for value-producing match and conditional expressions.

## 4. Verification And Tests

- [ ] 4.1 Run the LIR type checker on control-flow-lowered modules.
- [ ] 4.2 Add tests for `if`, `while`, `loop`, `break`, `continue`, restricted `for`, variants, and match lowering.
- [ ] 4.3 Add negative tests for invalid branch result types, invalid loop control placement, and invalid match payload extraction.
