const inflection = require('inflection');

const arrayFields = {
  data: true,
  backgroundColor: true,
  borderColor: true,
  borderWidth: true,
  hoverBackgroundColor: true,
  hoverBorderColor: true,
  hoverBorderWidth: true
}

function convert (graphqlData) {
  const data = {
    labels: [],
    datasets: [],
  };
  const dataSets = Object.keys(graphqlData);
  const numDataSets = dataSets.length;
  for (let i = 0; i < numDataSets; i++) {
    const dataSetName = dataSets[i];
    const dataSet = graphqlData[dataSetName];
    data.datasets.push({
      label: inflection.transform(dataSetName, ['underscore', 'humanize']),
    });
    const dataSetSize = dataSet.length;
    for (let j = 0; j < dataSetSize; j++) {
      const element = dataSet[j];
      Object.keys(element).forEach(property => {
        if (property === 'label') {
          if (i === 0) {
            data.labels.push(element[property]);
          }
        } else if (arrayFields[property]) {
          if (!data.datasets[i][property]) {
            data.datasets[i][property] = [];
          }
          data.datasets[i][property].push(element[property]);
        } else {
          data.datasets[i][property] = element[property];
        }
      });
    }
  }
  ;
  return data;
}

module.exports = convert;
