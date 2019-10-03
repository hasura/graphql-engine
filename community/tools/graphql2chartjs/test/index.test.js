const Graphql2Chartjs = require('../src/index');
const { vg1, vg2, vg3, scatter1, scatter2 } = require('./data.test')

const logTestResult = (condition, message) => {
  if (condition) {
    console.log(`Passed: ${message}`);
  } else {
    console.log(`Failed: ${message}`);
    process.exit(1);
  }
}

const runTests = () => {
  console.log('Running tests \n\n');
  let g2c = new Graphql2Chartjs();
  logTestResult((Object.keys(g2c.data).length === 0), 'Empty initialization');
  g2c = new Graphql2Chartjs()
  g2c.add(vg1.data, 'line')
  logTestResult(
    (
      g2c.data.labels.length === 5 && g2c.data.datasets.length === 1,
      g2c.data.datasets[0].fill === false &&
      g2c.data.datasets[0].data[0] === 427014 && g2c.data.datasets[0].pointBackgroundColor[0] === "red" &&
      g2c.data.datasets[0].data[1] === 220006 && g2c.data.datasets[0].pointBackgroundColor[1] === "yellow" &&
      g2c.data.datasets[0].data[2] === 71004 && g2c.data.datasets[0].pointBackgroundColor[2] === "#3366ff" &&
      g2c.data.datasets[0].data[3] === 129769 && g2c.data.datasets[0].pointBackgroundColor[3] === "#330000" &&
      g2c.data.datasets[0].data[4] === 90808 && g2c.data.datasets[0].pointBackgroundColor[4] === "green" &&
      true
    ),
    'Initialization with data without transformer'
  )
  g2c = new Graphql2Chartjs();
  g2c.add(vg2.data, (dsName, dp) => {
    return {
      ...dp, fill: true, chartType: 'line'
    }
  })
  logTestResult(
    (
      g2c.data.labels.length === 5 && g2c.data.datasets.length === 1,
      g2c.data.datasets[0].fill === true &&
      g2c.data.datasets[0].data[0] === 427014 && g2c.data.datasets[0].pointBackgroundColor[0] === "red" &&
      g2c.data.datasets[0].data[1] === 220006 && g2c.data.datasets[0].pointBackgroundColor[1] === "yellow" &&
      g2c.data.datasets[0].data[2] === 71004 && g2c.data.datasets[0].pointBackgroundColor[2] === "#3366ff" &&
      g2c.data.datasets[0].data[3] === 129222 && g2c.data.datasets[0].pointBackgroundColor[3] === "#330000" &&
      g2c.data.datasets[0].data[4] === 90808 && g2c.data.datasets[0].pointBackgroundColor[4] === "green"
    ),
    'Initialization with data with transformer'
  )
  g2c.add({ "VideoGameFollowers": [{
    "id": 4,
    "label": "PUBG",
    "data": 129769,
    "pointBackgroundColor": "#333333",
  }]}, 'line')
  logTestResult(
    (
      g2c.data.labels.length === 5 && g2c.data.datasets.length === 1,
      g2c.data.datasets[0].fill === true &&
      g2c.data.datasets[0].data[0] === 427014 && g2c.data.datasets[0].pointBackgroundColor[0] === "red" &&
      g2c.data.datasets[0].data[1] === 220006 && g2c.data.datasets[0].pointBackgroundColor[1] === "yellow" &&
      g2c.data.datasets[0].data[2] === 71004 && g2c.data.datasets[0].pointBackgroundColor[2] === "#3366ff" &&
      g2c.data.datasets[0].data[3] === 129769 && g2c.data.datasets[0].pointBackgroundColor[3] === "#333333" &&
      g2c.data.datasets[0].data[4] === 90808 && g2c.data.datasets[0].pointBackgroundColor[4] === "green"
    ),
    'Update without transformer'
  )
  g2c.add({ "VideoGameFollowers": [{
    "id": 4,
    "label": "PUBG",
    "data": 129769,
    "pointBackgroundColor": "#333333",
  }]}, (ds, dp) => {
    return {
      pointBackgroundColor: "#111111",
      data: 120000,
      chartType: 'line'
    }
  })
  logTestResult(
    (
      g2c.data.labels.length === 5 && g2c.data.datasets.length === 1 &&
      g2c.data.datasets[0].fill === true &&
      g2c.data.datasets[0].data[0] === 427014 && g2c.data.datasets[0].pointBackgroundColor[0] === "red" &&
      g2c.data.datasets[0].data[1] === 220006 && g2c.data.datasets[0].pointBackgroundColor[1] === "yellow" &&
      g2c.data.datasets[0].data[2] === 71004 && g2c.data.datasets[0].pointBackgroundColor[2] === "#3366ff" &&
      g2c.data.datasets[0].data[3] === 120000 && g2c.data.datasets[0].pointBackgroundColor[3] === "#111111" &&
      g2c.data.datasets[0].data[4] === 90808 && g2c.data.datasets[0].pointBackgroundColor[4] === "green"
    ),
    'Update with transformer'
  )
  g2c.add(scatter1.data, 'line');
  logTestResult(
    (
      g2c.data.labels.length === 5 && g2c.data.datasets.length === 3 &&
      g2c.data.datasets[1].backgroundColor === "purple" &&
      g2c.data.datasets[2].backgroundColor === "orange"
    ),
    'Update by adding a new dataset'
  )

  g2c.reform((dp, ds) => {
    return {
      chartType: 'line'
    }
  })

  g2c.add(scatter1.data, (ds, dp) => {
    if (ds === 'DataSet2') {
      return {
        ...dp,
        backgroundColor: 'red',
        chartType: 'line'
      };
    } else if (ds === 'DataSet1') {
      return {
        ...dp,
        backgroundColor: 'green',
        chartType: 'line'
      };

    }
    return dp;
  });
  logTestResult(
    (
      g2c.data.labels.length === 5 && g2c.data.datasets.length === 3 &&
      g2c.data.datasets[1].backgroundColor === "green" &&
      g2c.data.datasets[2].backgroundColor === "red"
    ),
    'Update by adding a new dataset with transformer'
  )

  g2c.reset(scatter1.data, (ds, dp) => {
    if (ds === 'DataSet2') {
      return {
        ...dp,
        backgroundColor: 'brown',
        chartType: 'scatter'
      };
    } else if (ds === 'DataSet1') {
      return {
        ...dp,
        backgroundColor: 'blue',
        chartType: 'bubble'
      };
    }
    return dp;
  });
  logTestResult(
    (
      g2c.data.labels.length === 0 && g2c.data.datasets.length === 2 &&
      g2c.data.datasets[1].backgroundColor === "brown" &&
      g2c.data.datasets[0].backgroundColor === "blue"
    ),
    'Reset scatter new data'
  )

  g2c.reset(vg3.data, (ds, db) => {
    return {
      chartType: 'bar'
    }
  })
  logTestResult(
    (
      g2c.data.labels.length === 5 && g2c.data.datasets.length === 1 &&
      g2c.data.datasets[0].backgroundColor[0] === "red" && g2c.data.datasets[0].data[0] === 427014 &&
      g2c.data.datasets[0].backgroundColor[1] === "yellow" && g2c.data.datasets[0].data[1] === 220006 &&
      g2c.data.datasets[0].backgroundColor[2] === "#3366ff" && g2c.data.datasets[0].data[2] === 71004 &&
      g2c.data.datasets[0].backgroundColor[3] === "#330000" && g2c.data.datasets[0].data[3] === 129769 &&
      g2c.data.datasets[0].backgroundColor[4] === "green" && g2c.data.datasets[0].data[4] === 90808
    ),
    'Reset with bar data'
  )

  g2c = new Graphql2Chartjs(vg1.data, 'line');
  logTestResult(
    (
      g2c.data.labels.length === 5 && g2c.data.datasets.length === 1,
      g2c.data.datasets[0].fill === false &&
      g2c.data.datasets[0].data[0] === 427014 && g2c.data.datasets[0].pointBackgroundColor[0] === "red" &&
      g2c.data.datasets[0].data[1] === 220006 && g2c.data.datasets[0].pointBackgroundColor[1] === "yellow" &&
      g2c.data.datasets[0].data[2] === 71004 && g2c.data.datasets[0].pointBackgroundColor[2] === "#3366ff" &&
      g2c.data.datasets[0].data[3] === 129769 && g2c.data.datasets[0].pointBackgroundColor[3] === "#330000" &&
      g2c.data.datasets[0].data[4] === 90808 && g2c.data.datasets[0].pointBackgroundColor[4] === "green" &&
      true
    ),
    'Initialization with constructor'
  )

  g2c = new Graphql2Chartjs(vg2.data, (dsName, dp) => {
    return {
      ...dp, fill: true, chartType: 'line'
    }
  })
  logTestResult(
    (
      g2c.data.labels.length === 5 && g2c.data.datasets.length === 1,
      g2c.data.datasets[0].fill === true &&
      g2c.data.datasets[0].data[0] === 427014 && g2c.data.datasets[0].pointBackgroundColor[0] === "red" &&
      g2c.data.datasets[0].data[1] === 220006 && g2c.data.datasets[0].pointBackgroundColor[1] === "yellow" &&
      g2c.data.datasets[0].data[2] === 71004 && g2c.data.datasets[0].pointBackgroundColor[2] === "#3366ff" &&
      g2c.data.datasets[0].data[3] === 129222 && g2c.data.datasets[0].pointBackgroundColor[3] === "#330000" &&
      g2c.data.datasets[0].data[4] === 90808 && g2c.data.datasets[0].pointBackgroundColor[4] === "green"
    ),
    'Initialization with constructor with transformer'
  )
}

runTests();
