# RFC: Support for Rename Symbol Operation in VSCode Extension using LSP

**Status**: Under implementation.

## Motivation

Say a user wants to rename a `DataConnectorLink`. Currently they will have to go through every `Model`, `ObjectType`, etc where the `DataConnectorLink` name is referred and do a lot of find and
replace. We can make use of Rename Symbol operation in the VSCode
extension for such use cases, which should find-and-replace all instances of the given `DataConnectorLink` intelligently.

The rename symbol operation is a fundamental feature in code editors like Visual Studio Code (VSCode). To enhance user experience and productivity, it's crucial to support this operation seamlessly within Hasura VSCode extension. This RFC proposes the necessary additions and modifications to achieve this functionality.

## Goals

- Enable users to rename symbols across project files consistently within VSCode.
- Ensure compatibility with the Language Server Protocol to maintain interoperability with VSCode and other LSP-compatible editors.

## Proposal

### 1. Protocol Extension

Extend the LSP to include support for the `textDocument/rename` method.

#### Request

```
{
  "method": "textDocument/rename",
  "params": {
    "textDocument": {
      "uri": "file:///path/to/document",
    },
    "position": {
      "line": 10,
      "character": 3
    },
    "newName": "newSymbolName"
  }
}
```

#### Response

```
{
  "changes": {
    "file:///path/to/document": [
      {
        "range": {
          "start": { "line": 10, "character": 0 },
          "end": { "line": 10, "character": 10 }
        },
        "newText": "newSymbolName"
      },
      // Additional changes if any
    ]
  }
}
```

### 2. Extension Implementation

##### a. Handle textDocument/rename Requests

- Implement the `textDocument/rename` request handler in the language server component of the VSCode extension.
- Parse the request parameters (`textDocument`, `position`, `newName`) to identify the symbol to be renamed and the new name.

#### b. Perform Symbol Renaming

- Perform a search within the identified scope to locate all occurrences of the symbol. This is done via a Hasura objects dependency tree stored on the lsp server, which stores data about which objects are inter-related.
- Generate a list of `TextEdit` objects that specify the changes required (i.e., new symbol name and its position).
- Aggregate these `TextEdit` objects into a response payload structured as shown in the response example above.

#### c. Send Response

- Send the response containing the changes object with all the necessary `TextEdit` entries back to the client (VSCode).

### 3. User Interaction

- In VSCode, trigger the rename symbol operation through the UI or a keyboard shortcut. [Image](https://drive.google.com/file/d/1v3VHN0onlqn1GDKWJQCbNxaQ0O2SHh-P/view?usp=sharing)
- Ensure the extension registers for and handles the `textDocument/rename` request appropriately to facilitate the rename operation seamlessly.

### Examples

- **Simple example**: Rename `DataConnectorLink.definition.name`. All references of this symbol should be renamed across all `Models`, `ObjectType`, `ObjectBooleanExpressionType` etc.

```
kind: DataConnectorLink
version: v1
definition:
  name: mypg  <---- Rename this, all references should get renamed
```

[Demo Video](https://drive.google.com/file/d/1f5CoDk1Xa3NeDKalEOT1ETl-SDhC8JDD/view?usp=sharing)

- **Complex example**: Rename `Model.definition.source.dataConnectorName`. A `Model` references `DataConnectorLink`. When user renames the `dataConnectorName` from within a `Model`, it should give similar result to the previous example and rename the `DataConnectorLink` object everywhere, and update all its references as well as the original `DataConnectorLink` object.

```
kind: Model
version: v1
definition:
  name: Orders
  objectType: Orders
  source:
    dataConnectorName: mypg  <---- Rename this
  ...


kind: DataConnectorLink
version: v1
definition:
  name: mypg    <----- This (and other references) should get renamed
```

[Demo Video](https://drive.google.com/file/d/1_zwCUqIbMG-E_rRI6bCDY_WWBv4AiYes/view?usp=sharing)

**Note**: Similar renames could be done with all objects which are referenced within other objects. For example, renaming a `field` from the `allowedFields` list of a `TypePermissions` object, should rename the original field in the `ObjectType`, as well as any other references of that `ObjectType`'s field everywhere in the metadata.

```
kind: TypePermissions
definition:
  typeName: Carts
  permissions:
    - role: admin
      output:
        allowedFields:
          - createdAt  <---- Rename this
...

kind: ObjectType
version: v1
definition:
  name: Carts
  fields:
    - name: createdAt  <---- This (and other references) should get renamed
      type: Timestamptz
```

- **Complicated example**: Rename `ModelPermissions.definition.permissions.role.select.filter.fieldComparison.operator`. The operator is coming directly from the `DataConnectorLink`. Here `ModelPermissions` declares the `modelName` which has an underlying `ObjectType`, whose field `countryOfOrigin` in the below example has a `ScalarType` (say `Text`) which is backed by a
  `DataConnectorScalarRepresentation` which links the `dataConnectorScalarType` to the graphql `ScalarType`. This `dataConnectorScalarType` (say `text`) is
  defined in `DataConnectorLink`'s schema under `scalar_types`, which also contains the `comparison_operators` that are allowed for this type, and `_eq` is one such operator. Renaming the `_eq` symbol here should rename the original `DataConnectorLink`'s schema and also any other references, where the `_eq` operator from the `text` type is used for the given `DataConnectorLink`.

```
kind: ModelPermissions
version: v1
definition:
  modelName: Products
  permissions:
    - role: admin
      select:
        filter:
          fieldComparison:
            field: countryOfOrigin
            operator: _eq    <------ Rename This
            value:
    ...


kind: Model
version: v1
definition:
  name: Products
  objectType: Products
...

kind: ObjectType
version: v1
definition:
  name: Products
  fields:
    - name: countryOfOrigin
      type: Text!
...

kind: ScalarType
version: v1
definition:
  name: Text
...

kind: DataConnectorScalarRepresentation
version: v1
definition:
  dataConnectorName: mypg
  dataConnectorScalarType: text
  representation: Text
...

kind: DataConnectorLink
version: v1
definition:
  name: mypg
  ...
  schema:
    ...
    scalar_types:
        text:
          representation:
          ...
          aggregate_functions:
          ...
          comparison_operators:
            _eq:     <------ This (and other references) should get Renamed
              type: equal
            ...

```

## Conclusion

Supporting the rename symbol operation within a VSCode extension using the Language Server Protocol enhances the editing experience by providing a standardized approach to renaming symbols across project files. This RFC proposes extending the Hasura LSP server with a `textDocument/rename` method and outlines the necessary implementation steps.
