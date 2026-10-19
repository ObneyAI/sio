# Structured output contracts

## Use one compiled contract for prompting and parsing

`sio.structured-output/compile-contract` takes a SIO specification and returns
the wire JSON Schema plus its paired local decoder. Send `:schema` as the
provider's response schema and `output-guide` as model-visible instructions.
Pass the unchanged compiled contract to `parse-output` for the returned content.
The caller remains responsible for provider finish/refusal handling.

Supported shapes are scalars, finite enums, nullable values, vectors and closed
finite maps. Literal keys, descriptions, constraints and enum meanings survive.
`:ordered-bands true` describes the declared enum ordering; false is an ordinary
enum. Invalid annotations and unsupported constraints fail before dispatch.
Open maps and ambiguous optional-nullable fields are explicitly unsupported.

## Presence and null

Required nullable fields must be supplied and retain null. Optional non-null
fields use required nullable wire slots: null decodes to absence. The paired
decoder rejects missing wire slots, extra keys, trailing content and invalid
values. It does not repair field names or generate answers.

The final marker and tool parsers now omit missing fields instead of inventing
nil values. Consumers must test key presence when distinguishing missing output
from an explicit null. `parse-streaming-output` retains nil placeholders for
required fields during incremental display; use the final parser before final
required-field validation. Nullable marker values decode complete JSON scalars
once, including quoted strings and null.

## Scope and verification

This branch also includes the existing input-field-guide changes: descriptions
follow the fields actually shown, including nested records and collections.
`specs/structured-output.allium` records the output contract. Its single
external-caller source-hint warning is intentional: callers belong to consuming
applications. Check/analyse have no errors or analysis findings.
