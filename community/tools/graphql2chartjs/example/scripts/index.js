require('dotenv').config();
const fetch = require('node-fetch');

const trendVars = {
  minor: 1,
  major: 10
};

let companies = [
  {ticker: 'AMZN', price: 821.66},
  {ticker: 'FB', price: 490.89},
  {ticker: 'AAPL', price: 853.23},
  {ticker: 'GOOG', price: 829.36},
  {ticker: 'MSFT', price: 871.95}
];

// Set up minor trend (updates every half second)
const changeMinor = () => {
  companies = companies.map((c) => {
    return {
      ticker: c.ticker,
      price: c.price + (Math.random() * 2) - 1
    };
  });
};
setInterval(changeMinor, 500);

// Set up major trend (updates every 5seconds)
const changeMajor = () => {
  companies = companies.map((c) => {
    return {
      ticker: c.ticker,
      price: c.price + (Math.random() * 20) - 10
    };
  });
};
setInterval(changeMajor, 5000);

// Update stock prices
const updatePrices = () => {
 fetch(
   'https://graphql2chartjs.hasura.app/v1/graphql',
   {
     method: 'POST',
     body: JSON.stringify({
       query: `
         mutation($data: [stocks_insert_input!]!) {
          insert_stocks(objects: $data) {
            affected_rows
          }
        }
       `,
       variables: {
         data: companies
       }
     }),
     headers: {
       'x-hasura-admin-secret': process.env.ADMIN_SECRET
     }
   }
 ).then((resp) => resp.json()).then((r) => {
   console.log(JSON.stringify(r, null, 2));
   setTimeout(updatePrices, 1000);
 });
};

updatePrices();
