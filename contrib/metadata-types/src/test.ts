import test from "ava"
import { default as yaml } from "js-yaml"
import { default as fs } from "fs"
import { default as path } from "path"
import { default as glob } from "fast-glob"

interface TestConfig {
  typeDefinitionFile: string
  jsonInputTests: Array<{
    files: string | string[]
    expectType: string
  }>
}

const pathToRoot = path.join(__dirname, "../")
const configFilePath = path.join(pathToRoot, "test-config.yaml")

async function collectTestInputs() {
  const configText = await fs.promises.readFile(configFilePath, "utf-8")
  const tests: TestConfig[] = yaml.load(configText)

  let results = []
  for (let entry of tests) {
    console.log("Running test for file", entry.typeDefinitionFile)
    const tsFilePath = path.join(pathToRoot, entry.typeDefinitionFile)

    for (let { files, expectType } of entry.jsonInputTests) {
      for (let file of await glob(files, { cwd: pathToRoot })) {
        console.log("Checking input data from", file)

        const filePath = path.join(pathToRoot, file)
        const data = await fs.promises.readFile(filePath, "utf-8")

        results.push({ file, data, tsFilePath, expectType })
      }
    }
  }

  return results
}

async function main() {
  for (let entry of await collectTestInputs()) {
    const testFileInfo = `[TYPE]: ${entry.tsFilePath} \n[INPUT]: ${entry.file}`

    test("Expect Pass & Get Valid Result \n" + testFileInfo, (t) => {
      t.notThrows(() => {
        const { Convert } = require(entry.tsFilePath)
        Convert["to" + entry.expectType](entry.data)
      }, "Converion from data to generated type failed")
    })
  }
}

main().catch((err) => console.log("ERR IN TESTS", err))
