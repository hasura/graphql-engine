const bar = require('./bar');
const line = require('./line');
const radar = require('./radar');
const bubble = require('./bubble');
const scatter = require('./scatter');
const polarArea = require('./polar-area');
const doughnutPie = require('./doughnut-pie');

function convert (type, graphqlData) {
  try {
    switch(type) {
      case 'line':
        return line(graphqlData);
        break;
      case 'bar':
        return bar(graphqlData);
        break;
      case 'radar':
        return radar(graphqlData);
        break;
      case 'bubble':
        return bubble(graphqlData);
        break;
      case 'polar-area':
        return polarArea(graphqlData);
      case 'doughnut':
        return doughnutPie(graphqlData);
      case 'pie':
        return doughnutPie(graphqlData);
      case 'scatter':
        return scatter(graphqlData);
      default:
        console.error('invalid chart type:', type)
        return {};
    }
  } catch (e) {
    console.error('unexpected error in graphql2chartjs; please check your graphql response');
    console.error(e);
  }
}

class Graphql2Chartjs {
  constructor(graphqlData, type, transformer) {
    this.handleInit(graphqlData, type,  transformer);
  }

  handleInit (graphqlData, type, transformer) {
    this.data = {};
    if (!graphqlData) {
      console.warn('Graphql2Chartjs is empty; use the update() method to add data');
    } else {
      if (!transformer) {
        this.gqlData = graphqlData;
        this.data = convert(type, graphqlData);
      } else {
        this.transformer = transformer;
        this.gqlData = this.transformGqlData(graphqlData, transformer);
        this.data = convert(type, this.gqlData);
      }
    }
  }

  transformGqlData(graphqlData, transformer) {
    const transformedGqlData = {};
    Object.keys(graphqlData).forEach(datasetName => {
      transformedGqlData[datasetName] = graphqlData[datasetName].map((dataPoint) => {
        return { ...dataPoint, ...transformer(datasetName, dataPoint) }
      });
    });
    return transformedGqlData;
  }

  reset (graphqlData, type, transformer) {
    this.handleInit(graphqlData, type, transformer);
  }

  update (graphqlData, type, transformer) {
    if (!graphqlData) {
      console.warn('invalid graphql data provided to Graphql2Chartjs');
      return;      
    }
    if (!this.gqlData || (this.gqlData && Object.keys(this.gqlData).length === 0)) {
      this.handleInit(graphqlData, type, transformer);
      return;
    }
    this.mergeData(
      type,
      transformer ? this.transformGqlData(graphqlData, transformer) : graphqlData
    );
  }

  mergeData(type, graphqlData) {
    const oldGqlData = { ...this.gqlData }
    Object.keys(graphqlData).forEach(dsName => {
      if (oldGqlData[dsName]) {
        graphqlData[dsName].forEach((dp) => {
          const oldDs = oldGqlData[dsName];
          let oldDsLength = oldGqlData[dsName].length;
          let refIndex;
          for (var _i = oldDs.length - 1; _i >= 0; _i--) {
            let refDp = oldDs[_i];
            if (refDp.label && refDp.label === dp.label) {
              refIndex = _i;
              break;
            } else if (refDp.data_r !== undefined) {
              if (refDp.data_x === dp.data_x && refDp.data_y === dp.data_y && refDp.data_r === dp.data_r) {
                refIndex = _i;
                break;
              }
            } else if (refDp.data_x !== undefined) {
              if (refDp.data_x === dp.data_x && refDp.data_y === dp.data_y) {
                refIndex = _i;
                break;
              }
            } else if (refDp.data_t !== undefined) {
              if (refDp.data_t === dp.data_t && refDp.data_y === dp.data_y) {
                refIndex = _i;
                break;
              }
            }
          }
          if (!refIndex) {
            refIndex = oldDsLength;
            oldDsLength++;
            oldGqlData[dsName] = [...oldGqlData[dsName], dp]
          } else {
            oldGqlData[dsName][refIndex] = {...oldGqlData[dsName][refIndex], ...dp};
          }
        })
      } else {
        oldGqlData[dsName] = graphqlData[dsName];
      }
    })
    this.gqlData = oldGqlData;
    this.data = convert(type, oldGqlData);
  }
}
if ((typeof window) != 'undefined') {
  window.Graphql2Chartjs = Graphql2Chartjs;
}
module.exports = Graphql2Chartjs;
