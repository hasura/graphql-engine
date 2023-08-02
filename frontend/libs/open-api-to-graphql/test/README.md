# Tests

We have a number of test suites used to verify the behavior of OpenAPI-to-GraphQL.

### Tests against real-world APIs

The following test suites perform a simple wrapping test and do not make call against any of the respective APIs.

| API                                                                                 | Test file                         |
| ----------------------------------------------------------------------------------- | --------------------------------- |
| N/A                                                                                 | `cloudfunction.test.ts`           |
| [DocuSign](https://www.docusign.com/)                                               | `docusign.test.ts`                |
| N/A                                                                                 | `government_social_work.test.ts`  |
| [IBM Language Translator](https://www.ibm.com/watson/services/language-translator/) | `ibm_language_translator.test.ts` |
| [Instagram](https://www.instagram.com/)                                             | `instagram.test.ts`               |
| [Stripe](https://stripe.com/)                                                       | `stripe.test.ts`                  |
| [Weather Underground](https://www.wunderground.com/)                                | `weather_underground_test.ts`     |

### Tests against custom APIs

We have created a number of example APIs for finer grain testing. Unfortunately, for a number of reasons including difficulty keeping tests on theme and some tests requiring their own specialized APIs, the number of tests have quickly grown and do not have meaningful identifiers.

The following table summarizes the purposes of these tests.

| Test file                | API(s)                                                                                                                                                                 | Testing purpose                                                                                                                                                                                                             |
| ------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `example_api.test.ts`    | `Example API`                                                                                                                                                          | An assortment of basic functionality and options on a company-themed API                                                                                                                                                    |
| `authentication.test.ts` | `Example API`                                                                                                                                                          | Basic authentication tests including using the [viewer functionality](../README.md#authentication)                                                                                                                          |
| `example_api2.test.ts`   | `Example API 2`                                                                                                                                                        | The [`operationIdFieldNames` option](../README.md#options)                                                                                                                                                                  |
| `example_api3.test.ts`   | `Example API` and `Example API 3`                                                                                                                                      | Creating GraphQL wrappers from multiple APIs and [interOAS links](../README.md#nested-objects)                                                                                                                              |
| `example_api4.test.ts`   | `Example API 4`                                                                                                                                                        | JSON schema [combining schema](https://json-schema.org/understanding-json-schema/reference/combining.html) keywords                                                                                                         |
| `example_api5.test.ts`   | `Example API 5`                                                                                                                                                        | The [`simpleNames` option](../README.md#options)                                                                                                                                                                            |
| `example_api6.test.ts`   | `Example API 6`                                                                                                                                                        | An assortment of other functionality and options                                                                                                                                                                            |
| `example_api7.test.ts`   | `Example API 7`                                                                                                                                                        | [Subscription support](../docs/subscriptions.md)                                                                                                                                                                            |
| `file_upload.test.ts`    | `File Upload API`                                                                                                                                                      | [File uploads](../README.md#file-uploads) and [file upload options](../README.md#options)                                                                                                                                   |
| `extensions.test.ts`     | `Extensions`, `Extensions Error 1`, `Extensions Error 2`, `Extensions Error 3`, `Extensions Error 4`, `Extensions Error 5`, `Extensions Error 6`, `Extensions Error 7` | The [`x-graphql-field-name`, `x-graphql-type-name`, and `x-graphql-enum-mapping` extensions](https://github.com/IBM/openapi-to-graphql/tree/master/packages/openapi-to-graphql#custom-type-and-field-names-and-enum-values) |
