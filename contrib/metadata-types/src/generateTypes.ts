import { promisify } from "util"
import _glob from "glob"
const glob = promisify(_glob)
import * as yaml from "js-yaml"
import * as path from "path"
import { promises as fs, readFileSync } from "fs"
import { schemaForTypeScriptSources } from "quicktype-typescript-input"
import {
  quicktype,
  InputData,
  JSONSchemaInput,
  JSONSchemaStore,
  RendererOptions,
  SerializedRenderResult,
  JSONSchemaSourceData,
} from "quicktype-core"
import assert from "assert"

/**
 * CONFIG
 */

const ALL_TYPES = "#/definitions/"
const pathFromRoot = path.join(__dirname, "..")
const configPath = path.join(pathFromRoot, "config.yaml")
const config: AppConfig = yaml.load(readFileSync(configPath, "utf-8"))

/**
 * This bit deals with converting what may potentially be an array of file paths
 * into separated glob expressions under the hood for the user
 */
const maybeConvertToMultiplePatterns = (object: string | string[]) =>
  Array.isArray(object) ? convertToMultiplePatterns(object) : object

const convertToMultiplePatterns = (array: string[]) =>
  array.length > 1 ? "{" + array.join(",") + "}" : array[0]

for (const key in config.input_files) {
  const value = config.input_files[key as SourceLanguage]
  config.input_files[key as SourceLanguage] = maybeConvertToMultiplePatterns(
    value
  )
}

const languageToExtensionMap = {
  "c++": "c++",
  crystal: "cr",
  csharp: "cs",
  dart: "dart",
  elm: "elm",
  flow: "js",
  go: "go",
  haskell: "hs",
  java: "java",
  kotlin: "kt",
  "objective-c": "m",
  pike: "pike",
  python: "py",
  ruby: "rb",
  rust: "rs",
  schema: "json",
  swift: "swift",
  typescript: "ts",
}

type Language = keyof typeof languageToExtensionMap
type SourceLanguage = "JsonSchema" | "Typescript"

interface AppConfig {
  output_directory: string
  selected_input_language: SourceLanguage
  input_files: Record<SourceLanguage, string>
  quicktype_config: Record<Language, RendererOptions>
}

interface TypegenOptions {
  source: SourceLanguage
  lang: string
  typeName: string
  file: string
  rendererOptions?: RendererOptions
}

type TypegenResult = {
  file: string
  results: SerializedRenderResult[]
}

/**
 * METHODS
 */

async function jsonSchemaToYAML(filename: string) {
  const filetext = await fs.readFile(filename, "utf-8")
  const jsonSchema = JSON.parse(filetext)
  const yamlSchema = yaml.dump(jsonSchema)
  fs.writeFile(filename.replace("json", "yaml"), yamlSchema, "utf-8")
}

async function getSchemaForFile(file: string, source: SourceLanguage) {
  switch (source) {
    case "JsonSchema":
      return fs.readFile(file, "utf-8")
    case "Typescript":
      return schemaForTypeScriptSources([file])
  }
}

// TODO: Make this take more than one file at a time, for resolving type imports in files
async function runTypeConversion(options: TypegenOptions) {
  const { source, lang, rendererOptions, typeName, file } = options
  let schema = await getSchemaForFile(file, source)

  // Dirty hack, if trying to convert from JSON Schema -> JSON Schema
  // We need to stop here and return schema directly, or else it'll mess up the output
  // by trying to convert it twice. So we fake a SerializedRenderResult manually.
  if (source == "JsonSchema" && lang == "schema") {
    const parsedSchema = JSON.stringify(JSON.parse(source as string), null, 2)
    const result: SerializedRenderResult = {
      lines: parsedSchema.split("\n"),
      annotations: [],
    }
    return result
  }

  //@ts-ignore
  const schemaInput = new JSONSchemaInput(new JSONSchemaStore())
  const inputData = new InputData()
  await schemaInput.addSource(schema as JSONSchemaSourceData)
  inputData.addInput(schemaInput)
  return quicktype({ inputData, rendererOptions, lang })
}

async function writeResult(
  file: string,
  lang: Language,
  result: SerializedRenderResult
) {
  const basename = path.basename(file, path.extname(file))
  const extension = languageToExtensionMap[lang]
  const filename = basename + "." + extension

  const text = result.lines.join("\n")
  const outpath = path.join(pathFromRoot, config.output_directory, filename)

  console.log("Wrote to:", outpath, "\n")
  return fs.writeFile(outpath, text, "utf-8")
}

async function generateTypes(source: SourceLanguage, file: string) {
  const entries = Object.entries(config.quicktype_config)

  let results: SerializedRenderResult[] = []
  for (let [lang, rendererOptions] of entries) {
    console.log("Generating types for language:", lang)
    const result = await runTypeConversion({
      source,
      lang,
      file,
      rendererOptions: rendererOptions ?? {},
      typeName: ALL_TYPES,
    })
    results.push(result)
    try {
      await writeResult(file, lang as Language, result)
    } catch (err) {
      console.log("[generateTypes]: error during writeResult", err)
    }
  }

  return results
}

/**
 * MAIN
 */

async function main(source: SourceLanguage) {
  const sourcePath = path.join(pathFromRoot, config.input_files[source])
  const files = await glob(sourcePath)

  let output: TypegenResult[] = []
  for (let file of files) {
    console.log("Starting codegen for file", file, "as source", source)
    const results = await generateTypes(source, file)
    output.push({ file, results })
  }

  return output
}

function exec() {
  const help = process.argv.includes("--help")
  const schema = process.argv.includes("--jsonschema")
  const typescript = process.argv.includes("--typescript")

  if (help) {
    console.log(`
    Usage: call script with only one of
      --jsonschema to generate types from JSON Schema definitions located in src/types/**.schema.json
      --typescript to generate types from Typescript definitions located in src/types/**.types.ts
    `)
    return
  }

  let lang = config.selected_input_language

  // If no lang passed in config.yaml, check one was passed as CLI
  if (!lang) {
    const atLeastOne = Boolean(typescript || schema)
    const butNotBoth = Boolean(typescript && schema) == false

    assert(atLeastOne, "Expected one of: --typescript | --jsonschema")
    assert(butNotBoth, "Expected ONLY one of: --typescript | --jsonschema")
  }

  // If CLI flags present, override language
  if (typescript) lang = "Typescript"
  if (schema) lang = "JsonSchema"

  return main(lang)
}

/**
 * INVOKE
 */

exec()
  ?.then((outputs) => {
    for (let output of outputs) {
      console.log("File:", output.file)
      console.log("Results:", output.results)
    }
  })
  .catch((err) => {
    console.log("got err in exec", err)
  })
  .finally(async () => {
    const generatedFolder = path.join(pathFromRoot, "generated", "/")
    const jsonSchemas = await glob(generatedFolder + "**.json")
    jsonSchemas.forEach(jsonSchemaToYAML)
  })
