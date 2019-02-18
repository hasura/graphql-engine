const bar = require('./bar');
const line = require('./line');
const radar = require('./radar');
const bubble = require('./bubble');
const scatter = require('./scatter');
const polarArea = require('./polar-area');
const doughnutPie = require('./doughnut-pie');

function convert (type, graphqlData) {
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
}

module.exports = convert
