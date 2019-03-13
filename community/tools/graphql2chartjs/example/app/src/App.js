import React from 'react';
import './App.css';
import NavBar from './Navbar';

import {
  BasicBarChart,
  StyledBarChart,
  MultiDatasetBarChart,
  MixedLineBarChart,
  LiveChart,
  RealtimeTimeseriesChart 
  // RealtimeTimeSeriesExample
} from './charts';

const App = () => {
  return (
    <div>
      <NavBar />
      <div style={{margin: '10px', paddingTop: '65px'}}>
        <BasicBarChart/>
        <StyledBarChart/>
        <MultiDatasetBarChart />
        <MixedLineBarChart />
        <LiveChart />
        <RealtimeTimeseriesChart />
        {
          /*
        <RealtimeLiveChartExample />
        <RealtimeTimeSeriesExample />
          */
        }
       </div>
    </div>
  )
}

export default App;
