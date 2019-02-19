# graphql2chartjs

> Realtime charts made easy with GraphQL and ChartJS

A tiny tool to restructure your GraphQL data as per the [ChartJS](https://chartjs.org) API thus leveraging Realtime GraphQL API to build Realtime charts.

![how convert works](assets/how_convert_works.png)

*Made with love by [Hasura](https://hasura.io)*

## Contents

- [Realtime](#realtime)
- [Live demo](#live-demo)
- [Getting started](#quick-start-with-react)
- [How it works](#how-it-works)
    + [Motivation](#motivation)
    + [GraphQL Aliasing](#graphql-aliasing)
    + [ChartJS API](#chartjs-api)
    + [The convert function](#the-convert-function)
    + [How the restructuring works](#how-the-restructuring-works)
- [Reference examples](#reference-examples)
    + [Bar](#bar)
    + [Line](#line)
    + [Radar](#radar)
    + [Pie](#pie)
    + [Doughnut](#doughnut)
    + [Bubble](#bubble)
    + [Scatter](#scatter)
- [Using views for aggregation](#using-views-for-aggregation)
- [Limitations](#limitations)

## Realtime

Realtime charts can be very useful in visualising live data trends. Two of the major use cases of realtime charts are:

1. Live time series
2. Realtime poll

You can see these two use cases implemented in [this section](#live-demo). We have used [Hasura GraphQL engine](https://hasura.io) as a realtime GraphQL backend. Hasura provides realtime GraphQL APIs over any Postgres database. Postgres is a good choice of a database for storing chart data because you can [create custom views](#using-views-for-aggregation) that aggregate the data in your tables. Hasura allows you to query (or subscribe to) these views over GraphQL. Hasura also allows you to have granular access control rules so that you can restrict the CRUD on your database based on user's session information.

## Live demo

**WIP**: An app running at some external URL showing a live query and a time series chart.

## Quickstart with React

1. Clone the example from the `examples` directory and install the dependencies.
    
    ```
    yarn install
    ```

2. **Run GraphQL Engine**: Run the Hasura GraphQL Engine and Postgres on Heroku's free tier (no credit card required) by clicking this button:

   [![Deploy to heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

   Note the URL. It will be of the form: `https://<app-name>.herokuapp.com`. Let's say it's `graphql2chartjs.herokuapp.com`.
   For instructions on how to deploy Hasura in other environments, head to the [docs](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html).

3. **Populate sample data**: Run the following two commands. (Replace `graphql-engine-url` with the URL you obtained above)
   
    ```sh
    export SAMPLE_JSON='{"video_games": [{ "id": 1, "name": "Dota", "followers": 427014, "color": "red"},{ "id": 2, "name": "CS:GO", "followers": 220006, "color": "yellow"},{ "id": 3, "name": "NFS", "followers": 71004, "color": "#3366ff"},{ "id": 4, "name": "PUBG", "followers": 129769, "color": "#330000"},{ "id": 5, "name": "Quake 3", "followers": 90808, "color": "green"}]}'
    echo $SAMPLE_JSON > db.json
    npx json2graphql <graphql-engine-url> -d db.json
    ```

4. **Set constants in the app**: Go to `src/constants.js` and set HGE_URL to your GraphQL Engine URL.

5. **Run the app**:
    ```
    yarn start
    ```
    
The app opens at `localhost:3000` rendering the realtime bar chart.

## How it works

### Motivation

We started using ChartJS with GraphQL so that we could leverage GraphQL's realtime subscriptions to build realtime charts. Soon enough we realised that we can automate this tedious procedure of restructuring the GraphQL data to a form that ChartJS expects.

The idea behind this tool is to generate ChartJS compliant `data` object from your GraphQL response by simply adding a few aliases in your GraphQL query.

### GraphQL Aliasing

GraphQL gives you the power of aliasing the response fields with custom names. Lets look at a simple GraphQL query.

```gql
query {
    rootField {
        field1
        field2
    }
}

```

The response to this query would be of the form:

```json
{
    "data": {
        "rootField": [
            {
                "field1": "value 1",
                "field2": "value 2"
            }
        ]
    }
}
```

Now, when we alias the above GraphQL query like so:

```gql
query {
    aliasedRootField: rootField {
        aliasedField1: field1
        aliasedField2: field2
    }
}
```

The response would be:

```
{
    "data": {
        "aliasedRootField": {
            "aliasedField1": 'value 1',
            "aliasedField2": 'value 2'
        }
    }
}
```

### ChartJS API

Most of the ChartJS charts expect a data object of the form:

```js
{
    "labels": ["label1", "label2", ..., "label10"], // list of strings
    "datasets": [ // list of custom datasets with their properties
        {
            "data": [1334, 4314, ..., 2356],
            "backgroundColor": ['red', "blue", ..., "brown"],
            "borderColor": ['red', "blue", ..., "brown"],
            "fill": false
        } 
    ]
}
```

### The convert function

The `convert` function i.e. the default export of this library accepts two arguments:
1. **type**: (String) Type of the chart; Eg. `bra`, `line`, `pie`
2. **graphqlData**: [Object] This should be an object with each field having its value as a list of data points.

You can directly feed the output of the `convert` function to your ChartJS instance.

```js

const graphQLResponse = makeGraphQLQuery();
var chartType = 'bar';

var myChart = new Chart(ctx, {
    type: chartType,
    data: convert(chartType, graphQLResponse),
    options: {...} //custom options
});

```

### How the restructuring works

The convert function understands the API for each kind of chart that it supports. It constructs appropriate arrays mapping the indices of labels with other dataset properties.

Lets consider this GraphQL response:

```json
{
  "data": {
    "VideoGameFollowers": [
      {
        "id": 1,
        "label": "Dota",
        "data": 427014,
        "pointBackgroundColor": "red",
        "fill": false
      },
      {
        "id": 2,
        "label": "CS:GO",
        "data": 220006,
        "pointBackgroundColor": "yellow",
        "fill": false
      },
      {
        "id": 3,
        "label": "NFS",
        "data": 71004,
        "pointBackgroundColor": "#3366ff",
        "fill": false
      },
      {
        "id": 4,
        "label": "PUBG",
        "data": 129769,
        "pointBackgroundColor": "#330000",
        "fill": false
      },
      {
        "id": 5,
        "label": "Quake 3",
        "data": 90808,
        "pointBackgroundColor": "green",
        "fill": false
      }
    ]
  }
}
```

The above GraphQL response is restructured to the ChartJS `data` as follows:

1. It starts with initializing the `data` object as:

    ```json
    {
        "labels": [],
        "datasets": []
    }
    ```

2. It pushes a dataset with label as `humanized(rootFieldName)`. In this case, the root field is `VideoGameFollowers`. After inserting this step, the `data` object looks like 

    ```json
    {
        "labels": [],
        "dataset": [
            {
                "label": "Video game followers"
            }
        ]
    } 
    ```

3. It then iterates over the contents of this dataset. For each datapoint in the dataset, it pushes the label to the top level `labels` array and every other property to the dataset. So, after inserting the first data point, that is:
    ```json
    {
      "id": 1,
      "name": "Dota",
      "data": 427014,
      "pointBackgroundColor": "red",
      "fill": false
    }
    ```

    the `data` object looks like:

    ```json
    {
        "labels": ["Dota"],
        "datasets": [
            {
                "data": [427014],
                "pointBackgroundColor": ["red"]
                "fill": false
            }
        ]
    }
    ```

    As you see, `pointBackgroundColor` and `data` get pushed in an array while `fill` gets set as a top level field. This is because `convert` function understands that the ChartJS API expects `pointBackgroundColor` to be an array and `fill` to be a simple flag.

4. It repeats the step above for every data point. The final `data` object would be:

    ```json
    {
      "labels": [ "Dota", "Cs:go", "Nfs", "Pubg", "Quake 3"],
      "datasets": [
        {
          "label": "Video game followers",
          "id": 5,
          "data": [ 427014, 220006, 71004, 129769, 90808 ],
          "pointBackgroundColor": ["red", "yellow", "#3366ff", "#330000", "green"],
          "fill": false
        }
      ]
    }
    ```

Now you can pass this data object to your ChartJS instance and you will have a chart like this:

![line-chart-example](assets/readme-line-chart-example.png)

## Reference examples:

### Bar

```js
var graphQLResponse = makeGraphQLQuery(...);

/*
    {
      "data": {
        "VideoGameFollowers": [
          {
            "id": 1,
            "label": "Dota",
            "data": 427014,
            "backgroundColor": "red",
          },
          {
            "id": 2,
            "label": "CS:GO",
            "data": 220006,
            "backgroundColor": "yellow",
          },
          {
            "id": 3,
            "label": "NFS",
            "data": 71004,
            "backgroundColor": "#3366ff",
          },
          {
            "id": 4,
            "label": "PUBG",
            "data": 129769,
            "backgroundColor": "#330000",
          },
          {
            "id": 5,
            "label": "Quake 3",
            "data": 90808,
            "backgroundColor": "green",
          }
        ]
      }
    }
*/

var chartJSData = convert('bar', graphQLResponse.data)

var myChart = new Chart(ctx, {
    type: 'bar',
    data: chartJSData,
    options: {
        responsive: true,
        legend: {
            labels: {
                boxWidth: 0,
                hidden: true
            }
        }
    }
})
```

![readme bar chart example](assets/readme-bar-chart-example.png)

### Line

```js
var graphQLResponse = makeGraphQLQuery(...);

/*
    {
      "data": {
        "VideoGameFollowers": [
          {
            "id": 1,
            "label": "Dota",
            "data": 427014,
            "pointBackgroundColor": "red",
            "fill": false
          },
          {
            "id": 2,
            "label": "CS:GO",
            "data": 220006,
            "pointBackgroundColor": "yellow",
            "fill": false
          },
          {
            "id": 3,
            "label": "NFS",
            "data": 71004,
            "pointBackgroundColor": "#3366ff",
            "fill": false
          },
          {
            "id": 4,
            "label": "PUBG",
            "data": 129769,
            "pointBackgroundColor": "#330000",
            "fill": false
          },
          {
            "id": 5,
            "label": "Quake 3",
            "data": 90808,
            "pointBackgroundColor": "green",
            "fill": false
          }
        ]
      }
    }
*/

var chartJSData = convert('line', graphQLResponse.data)

var myChart = new Chart(ctx, {
    type: 'line',
    data: chartJSData,
    options: {
        responsive: true,
        legend: {
            labels: {
                boxWidth: 0,
                hidden: true
            }
        }
    }
})

```

![line-chart-example](assets/readme-line-chart-example.png)

### Radar

```js
var graphQLResponse = makeGraphQLQuery(...);

/*
   {
    "data": {
      "VideoGameFollowers": [
        {
          "id": 1,
          "label": "Dota",
          "data": 427014,
          "backgroundColor": "rgba(100, 0, 0, 0.3)",
          "pointBackgroundColor": "red",
          "fill": true
        },
        {
          "id": 2,
          "label": "CS:GO",
          "data": 220006,
          "backgroundColor": "rgba(100, 0, 0, 0.3)",
          "pointBackgroundColor": "yellow",
          "fill": true
        },
        {
          "id": 3,
          "label": "NFS",
          "data": 71004,
          "backgroundColor": "rgba(100, 0, 0, 0.3)",
          "pointBackgroundColor": "#3366ff",
          "fill": true
        },
        {
          "id": 4,
          "label": "PUBG",
          "data": 129769,
          "backgroundColor": "rgba(100, 0, 0, 0.3)",
          "pointBackgroundColor": "#330000",
          "fill": true
        },
        {
          "id": 5,
          "label": "Quake 3",
          "data": 90808,
          "backgroundColor": "rgba(100, 0, 0, 0.3)",
          "pointBackgroundColor": "green",
          "fill": true
        }
      ]
    }
  }
*/

var chartJSData = convert('radar', graphQLResponse.data)

var myChart = new Chart(ctx, {
    type: 'radar',
    data: chartJSData,
    options: {
        responsive: true,
        legend: {
            labels: {
                boxWidth: 0,
                hidden: true
            }
        }
    }
})
```

![readme example radar](assets/readme-radar-chart-example.png)

### Pie

```js
var graphQLResponse = makeGraphQLQuery(...);

/*
   {
    "data": {
      "VideoGameFollowers": [
        {
          "id": 1,
          "label": "Dota",
          "data": 427014,
          "backgroundColor": "red",
          "borderColor": "lightred"
        },
        {
          "id": 2,
          "label": "CS:GO",
          "data": 220006,
          "backgroundColor": "yellow",
          "borderColor": "lightyellow"
        },
        {
          "id": 3,
          "label": "NFS",
          "data": 71004,
          "backgroundColor": "#3366ff",
          "borderColor": "lightgreen"
        },
        {
          "id": 4,
          "label": "PUBG",
          "data": 129769,
          "backgroundColor": "#330000",
          "borderColor": "#42SF22"
        },
        {
          "id": 5,
          "label": "Quake 3",
          "data": 90808,
          "backgroundColor": "green",
          "borderColor": "#5ACD23"
        }
      ]
    }
  }
*/

var chartJSData = convert('pie', graphQLResponse.data)

var myChart = new Chart(ctx, {
    type: 'pie',
    data: chartJSData,
    options: {
        responsive: true
    }
})
```

![readme example radar](assets/readme-pie-chart-example.png)

### Doughnut

```js
var graphQLResponse = makeGraphQLQuery(...);

/*
   {
    "data": {
      "VideoGameFollowers": [
        {
          "id": 1,
          "label": "Dota",
          "data": 427014,
          "backgroundColor": "red",
          "borderColor": "lightred"
        },
        {
          "id": 2,
          "label": "CS:GO",
          "data": 220006,
          "backgroundColor": "yellow",
          "borderColor": "lightyellow"
        },
        {
          "id": 3,
          "label": "NFS",
          "data": 71004,
          "backgroundColor": "#3366ff",
          "borderColor": "lightgreen"
        },
        {
          "id": 4,
          "label": "PUBG",
          "data": 129769,
          "backgroundColor": "#330000",
          "borderColor": "#42SF22"
        },
        {
          "id": 5,
          "label": "Quake 3",
          "data": 90808,
          "backgroundColor": "green",
          "borderColor": "#5ACD23"
        }
      ]
    }
  }
*/

var chartJSData = convert('doughnut', graphQLResponse.data)

var myChart = new Chart(ctx, {
    type: 'doughnut',
    data: chartJSData,
    options: {
        responsive: true
    }
})
```

![readme example radar](assets/readme-dougnut-chart-example.png)

### Bubble (multiple datasets)

```js
var graphQLResponse = makeGraphQLQuery(...);

/*
  {
    "data": {
      "DatSet1": [
        {
          "id": 1,
          "data_x": 24,
          "data_y": 45,
          "data_r": 11,
          "backgroundColor": "red"
        },
        {
          "id": 1,
          "data_x": 54,
          "data_y": 34,
          "data_r": 9,
          "backgroundColor": "red"
        },
        {
          "id": 1,
          "data_x": 14,
          "data_y": 67,
          "data_r": 12,
          "backgroundColor": "red"
        },
        {
          "id": 1,
          "data_x": 54,
          "data_y": 5,
          "data_r": 16,
          "backgroundColor": "red"
        },
      ],
      "DatSet2": [
        {
          "id": 1,
          "data_x": 92,
          "data_y": 55,
          "data_r": 4,
          "backgroundColor": "green"
        },
        {
          "id": 1,
          "data_x": 32,
          "data_y": 87,
          "data_r": 17,
          "backgroundColor": "green"
        },
        {
          "id": 1,
          "data_x": 45,
          "data_y": 85,
          "data_r": 9,
          "backgroundColor": "green"
        },
        {
          "id": 1,
          "data_x": 24,
          "data_y": 50,
          "data_r": 12,
          "backgroundColor": "green"
        },
      ]
    }
  }  
*/

var chartJSData = convert('bubble', graphQLResponse.data)

var myChart = new Chart(ctx, {
    type: 'bubble',
    data: chartJSData,
    options: {
        responsive: true
    }
})
```

![readme example bubble](assets/readme-bubble-chart-example.png)

### Scatter (multiple datasets)

```js
var graphQLResponse = makeGraphQLQuery(...);

/*
  {
    "data": {
      "DatSet1": [
        {
          "id": 1,
          "data_x": 24,
          "data_y": 45,
          "backgroundColor": "red"
        },
        {
          "id": 1,
          "data_x": 54,
          "data_y": 34,
          "backgroundColor": "red"
        },
        {
          "id": 1,
          "data_x": 14,
          "data_y": 67,
          "backgroundColor": "red"
        },
        {
          "id": 1,
          "data_x": 54,
          "data_y": 5,
          "backgroundColor": "red"
        },
      ],
      "DatSet2": [
        {
          "id": 1,
          "data_x": 92,
          "data_y": 55,
          "backgroundColor": "green"
        },
        {
          "id": 1,
          "data_x": 32,
          "data_y": 87,
          "backgroundColor": "green"
        },
        {
          "id": 1,
          "data_x": 45,
          "data_y": 85,
          "backgroundColor": "green"
        },
        {
          "id": 1,
          "data_x": 24,
          "data_y": 50,
          "backgroundColor": "green"
        }
      ]
    }
  }  
*/

var chartJSData = convert('scatter', graphQLResponse.data)

var myChart = new Chart(ctx, {
    type: 'scatter',
    data: chartJSData,
    options: {
        responsive: true
    }
})
```

![readme example scatter](assets/readme-scatter-chart-example.png)

## Using views for aggregation

Sometimes you would want to aggregate data from your tables in Postgres.
/WIP


## Limitations

1. The response structure of your GraphQL query is opinionated. This is however not a limitation as you use GraphQL aliasing to renaeme the fields to achieve the required structure.

2. GraphQL spec for subscriptions allows subscribing to just one root field. This doesn't allow for having multiple datasets in the chart. The workaround for this is to open multiple subscriptions and form a custom GraphQL response from the subscription responses.
