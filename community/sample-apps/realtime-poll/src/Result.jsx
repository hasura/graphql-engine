import { useSubscription } from "@apollo/client";
import React from "react";
import { Chart } from "react-google-charts";
import { Error, Loading } from "./Components";
import { SUBSCRIPTION_RESULT } from "./GraphQL";

export const Result = ({ pollId }) => {
  const { data, loading, error } = useSubscription(SUBSCRIPTION_RESULT, {
    variables: { pollId },
  });

  const hasResults = data?.poll_results.length > 0;

  if (loading) return <Loading />;
  if (error) return <Error message={error.message} />;

  return (
    <div>
      {hasResults ? <PollChart data={data?.poll_results} /> : <p>No result</p>}
    </div>
  );
};

const PollChart = ({ data }) => {
  const COLOR = "color: #4285f4";
  const d = [
    ["Option", "No. of votes", { role: "annotation" }, { role: "style" }],
  ];

  data.forEach(({ option, votes }) =>
    d.push([option.text, parseInt(votes), parseInt(votes), COLOR])
  );

  return (
    <Chart
      className="poll-result-chart-container"
      chartType="BarChart"
      loader={<div>Loading Chart</div>}
      data={d}
      options={chartOptions}
    />
  );
};

const chartOptions = {
  height: "100%",
  chart: {
    title: "Realtime results",
  },
  legend: { position: "none" },
  animation: {
    duration: 1000,
    easing: "out",
    startup: true,
  },
};
