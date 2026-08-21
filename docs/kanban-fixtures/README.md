# Kanban Roundtrip Fixtures

Each fixture is a pair: `input.wiki` (the original) and `golden.wiki` (the expected
text → model → text serialization).

What the round trip must preserve:

- card IDs
- list and card order
- text outside the Kanban block, byte for byte
