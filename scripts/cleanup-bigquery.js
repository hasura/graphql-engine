// Delete a bunch of bigquery test datasets.
//
// Note that we can only select the first 50 datasets at a time, so you might
// have to run this a few times to clear it out completely.

const { exec } = require('child_process')
const util = require('util')

const allDatasetsCommand = "bq ls --project_id regency-polecat-beehive --format=prettyjson | jq '.[]  | .datasetReference.datasetId | select(startswith(\"hasura_test_\"))'"
const singleDatasetCommand = dataset => `bq show --format=prettyjson regency-polecat-beehive:${dataset}`
const removeDatasetCommand = dataset => `bq rm --dataset=true --force=true --recursive=true regency-polecat-beehive:${dataset}`

const now = Date.now()

const removeDataset = dataset =>
  util.promisify(exec)(removeDatasetCommand(dataset))
    .then(_ => console.log('Deleted ' + dataset))

const checkDataset = dataset =>
  util.promisify(exec)(singleDatasetCommand(dataset))
    .then(({ stdout }) => {
      const parsed = JSON.parse(stdout)
            lastUpdate = parsed.lastModifiedTime

      if (now - (1000 * 60 * 60 * 6) > lastUpdate) {
        console.log('Deleting ' + dataset)
        removeDataset(dataset)
      } else {
        console.log('Not deleting ' + dataset)
      }
    })
    .catch(_ => console.log('Skipping ' + dataset))

util.promisify(exec)(allDatasetsCommand)
  .then(({ stdout }) => {
    const datasets =
            stdout
              .split('\n')
              .filter(x => x != '')
              .map(x => x.substring(1, x.length - 1))

    console.log('Found ' + datasets.length + ' datasets')
    datasets.map(checkDataset)
  })
