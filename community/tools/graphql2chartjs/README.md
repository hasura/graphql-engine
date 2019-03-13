# graphql2chartjs - Instant realtime charts using GraphQL

`graphql2chartjs` reshapes your GraphQL data as per the [ChartJS](https://chartjs.org) API. This makes it easy to query a GraphQL API and render the output as a ChartJS chart.

For example, if you're using Postgres and [Hasura](https://hasura.io), this is what using `graphql2chartjs` looks like:

![graphql2chartjs](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/img/graphql2chartjs-explained.png)



## Demos & sandbox
We've set up a GraphQL server with continuously changing data, so that you can try graphql2chartjs out easily.


|[View live charts](https://graphql2chartjs-examples.herokuapp.com) | [Edit in sandbox](https://codesandbox.io/s/p2wpj1o8pj) | [Open GraphiQL](https://g2c-examples-graphiql.herokuapp.com/) |
|---|---|---|

![realtime chart with live data](https://storage.googleapis.com/graphql-engine-cdn.hasura.io/assets/graphql2chartjs/live-chart.gif)

The demo above cover the following types of charts: [basic](https://graphql2chartjs-examples.herokuapp.com/#bar), [multiple datasets](https://graphql2chartjs-examples.herokuapp.com/#multi-bar), [mixed chart-types](https://graphql2chartjs-examples.herokuapp.com/#mixed), [realtime chart with live data](https://graphql2chartjs-examples.herokuapp.com/#live-chart), [realtime time-series](https://graphql2chartjs-examples.herokuapp.com/#timeseries-chart)

## Usage with Hasura
Hasura gives you an instant realtime GraphQL API on an existing Postgres database. You can create views to capture analytics and aggregations on your database and instantly turn them into charts.

Watch this video below to see a demo/tutorial of using Hasura on an existing Postgres database, creating views and using `graphql2chartjs` to build realtime charts in under 5 minutes.

<div style="text-align:center">
  <a href="https://www.youtube.com/watch?v=153iv1-qFuc&feature=youtu.be" target="_blank">
    <img src="https://storage.googleapis.com/graphql-engine-cdn.hasura.io/assets/graphql2chartjs/g2c-youtube-embed.png" width="1000px" alt="youtube video demo">
  </a>
</div>


## Example usage with react, apollo and react-chartjs-2

```javascript
import {Query} from 'react-apollo';
import gql from 'graphql-tag';
import Graphql2Chartjs from 'graphql2chartjs';
import {Bar} from 'react-chartjs-2';

const g2c = new Graphql2Chartjs();
<Query 
  query={gql`
    query {
      Articles: articleStats {
        label: title
        data: num_likes
      }
    }`}
  }>
  {({data} => {
    if (data) {
      g2c.add(data, 'bar');
      return (<Bar data={g2c.data} />);
    }
    return null;
  }
</Query>
```

## Mapping GraphQL queries to ChartJS charts

Different types of charts need different structures in their datasets. 

For example a bar chart dataset needs labels and data associated for each label; the ChartJS API refers to this as `label` and `data`. Once you alias fields in your graphql query to `label` and `data`, and pass the response through `graphql2chartjs`, your dataset is ready to be used by bar chart in chartjs.

### Bar / Line / Doughnut / Pie / Radar / Polar Area / Area

Charts of this type need 2 data inputs, `label` and `data`.
```graphql
query {
  ArticleLikes : articles {
    label: title
    data: likes
  }
}
```

### Scatter / Bubble

Charts of this type need 2 data inputs: `data_x`, `data_y` (and `data_r` for bubble).
```graphql
query {
  ArticleLikesVsComments : articles {
    data_x: num_likes
    data_y: num_comments
  }
}
```

### Time series (line / bar)

Charts of this type need 2 data inputs, `data_x` or `data_t` and `data_y`. Note that there is no `label`.

```graphql
query {
  StockPrices : stockprice {
    data_t: created
    data_y: price
  }
}
```

## graphql2chartjs usage

graphql2chartjs works in 3 steps:

1. Initialise graphql2chartjs: `const g2c = new Graphql2Chartjs()`
2. Add data from your graphql response: `g2c.add(graphqlResponse.data, 'line')`
3. Set your chart data to the data properly of the graphql2chartjs instance: `g2c.data`

### Step 1: Initialiase - `new Graphql2Chartjs()`

```javascript
const g2c = new Graphql2Chartjs();
```

### Step 2: (Option 1) Add data for your chart - `g2c.add(graphqlResponse.data, chartType)`

Once you've initialised a `graphql2chartjs` object, you can use the `add` function to add data for the first time or incrementally:

```javascript
await data = runQuery(..);

g2c.add(data, 'line');
```

**Arguments:**

- `data`: This is your GraphQL response. This data should have fields `label`, `data` etc. as per the GraphQL querying described above.
- `chartType`: This is a string that represents valid values of what your chart type is. Valid values include `'line'`, `'bar'`, `'radar'`, `'doughnut'`, `'pie'`, `'polarArea'`, `'bubble'`, `'scatter'`.

**Notes:**
- This is the simplest way of using `graphql2chartjs`
- If you have multiple datasets, all of the datasets will be rendered automatically as the same type of chart
- To customise the UI options of the rendered chart like colors or to create a mixed type chart (one dataset is rendered as a line chart, another as a bar chart) use the `addWithProps` function instead of this one.


### Step 2: (Option 2) Add data for your chart with UI properties - `graphql2chartjs.add(data, addProps())`

Once you've initialised a `graphql2chartjs` object, you can use the `add` function to add data for the first time or incrementally. In addition, you can pass a function that specifies the type of chart, chartjs UI properties that apply to the entire dataset or to each point in the dataset.

```javascript
await data = runQuery(..);

g2c.add(data, (datasetName, dataPoint) => {
  return {                                                                                                                  
    chartType: 'line',
    pointBackgroundColor: 'blue',
    borderColor: 'red'
  };
});
```

**Arguments:**

- `data`: This is an array of objects or an object from your graphql response. This data should have fields `label`, `data` etc. as per the GraphQL querying described above.
- `addProps(datasetName, dataPoint)`: This function defined by you can take the name of the dataset and the data record that comes from the GraphQL response and returns an object that can should have the `chartType` key and optionally other keys that specify other dataset properties.
  - The object returned by this function should look like the following:
  ```javascript
  {
    chartType: 'line', // Or 'line', 'bar', 'radar', 'doughnut', 'pie', 'polarArea', 'bubble', 'scatter'
    <other keys as per the dataset properties per chart. Refer to the link below>
  }
  ```
  - `chartType`: This should be a string value, one of: `'line'`, `'bar'`, `'radar'`, `'doughnut'`, `'pie'`, `'polarArea'`, `'bubble'`, `'scatter'`
  - Other keys in this object should be dataset properties. These properties are slightly different for different chart types. 
    - Line chart: https://www.chartjs.org/docs/latest/charts/line.html#dataset-properties
    - Bar chart: https://www.chartjs.org/docs/latest/charts/bar.html#dataset-properties
    - Radar chart: https://www.chartjs.org/docs/latest/charts/radar.html#dataset-properties
    - Doughnut & Pie: https://www.chartjs.org/docs/latest/charts/doughnut.html#dataset-properties
    - Polar: https://www.chartjs.org/docs/latest/charts/polar.html#dataset-properties
    - Bubble: https://www.chartjs.org/docs/latest/charts/bubble.html#dataset-properties
    - Scatter: https://www.chartjs.org/docs/latest/charts/scatter.html#dataset-properties

### Step 3: Now create your chart with data - `g2c.data`

Now that your data is created by `g2c.add()` or `g2c.addWithProps()`, use it to build your chart!

`g2c.data` gives you access to the latest chartJS data that can be passed to your chart.

1. New chart:
  ```javascript
  import Chart from 'chart.js';

  var ctx = document.getElementById("myChart");

  var myChart = new Chart(ctx, { data: g2c.data });
  ```
2. Updating chart:
  ```javascript
  // Run a query to fetch new data
  await newData = runQuery(...);
  
  // Add new data that will get incrementally added to existing data in your graphql2chartjs instance
  g2c.add(newData, 'line');

  // Set your chart to this new data and update!
  myChart.data = g2c.data;
  myChart.update();
  ```

## Installation

### Via npm

```
npm install --save graphql2chartjs
```

### Use in a script tag

```html
<script src="https://cdn.jsdelivr.net/gh/hasura/graphql-engine/master/community/tools/graphql2chartjs/bundle/js/index.min.js" type="text/javascript"></script>
```

## Reforming the data

### `reform()`

You can reform the existing data in your `graphql2chartjs` instance using the reform function that takes a reformer function as an argument. This reformer function is run over every datapoint in every dataset. For instance, to scale the x and y coordinates, you would do something like:

```
g2c.reform((datasetName, dataPoint) => {
  // scale the x, y coordinates
  return {
    data_x: scalingFactor(dataPoint.data_x),
    data_y: scalingFactor(dataPoint.data_y)
  }
})
```
