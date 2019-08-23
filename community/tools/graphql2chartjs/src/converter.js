const inflection = require('inflection');
const arrayFields = require('./array-fields');

const chartTypeMap = {
  'line': arrayFields.line,
  'bar': arrayFields.bar,
  'radar': arrayFields.radar,
  'polarArea': arrayFields.polar,
  'doughnut': arrayFields.pie,
  'pie': arrayFields.pie,
  'bubble': arrayFields.bubble,
  'scatter': arrayFields.scatter
};

function convert(graphqlData, chartType) {
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
      data: []
    });
    const dataSetSize = dataSet.length;
    for (let j = 0; j < dataSetSize; j++) {
      const element = dataSet[j];
      let isRadiusDefined = element.data_r !== undefined;
      if (element.data_x !== undefined || element.data_t !== undefined) {
        if (element.data_x) {
          const dataPoint = {
            x: element.data_x,
            y: element.data_y,
          }
          if (isRadiusDefined) {
            dataPoint.r = element.data_r;
          }
          data.datasets[i].data.push(dataPoint);
        } else if (element.data_t !== undefined) {
          data.datasets[i].data.push();
          let dataPoint = {
            t: element.data_t,
            y: element.data_y
          };
          if (isRadiusDefined) {
            dataPoint[r] = element.data_r;
          }
          data.datasets[i].data.push(dataPoint);
        }
      }
      const arrayFieldsByType = element.chartType ? chartTypeMap[element.chartType] : chartTypeMap[chartType];
      Object.keys(element).forEach(property => {
        if (property === 'data_x' || property === 'data_t' || property === 'data_y' || property === 'data_r') {
          return;
        }
        if (property === 'label') {
          if (i === 0) {
            data.labels.push(element[property]);
          }
        } else if (arrayFieldsByType[property]) {
          if (!data.datasets[i][property]) {
            data.datasets[i][property] = [];
          }
          data.datasets[i][property].push(element[property]);
        } else {
          data.datasets[i][property === 'chartType' ? 'type' : property] = element[property];
        }
      });
    }
  };
  return data;
}

module.exports = convert;
