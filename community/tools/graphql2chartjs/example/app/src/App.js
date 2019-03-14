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
} from './charts';

const App = () => (
  <div>
    <NavBar />
    <div style={{margin: '10px', paddingTop: '65px'}}>
      <BasicBarChart/>
      <StyledBarChart/>
      <MultiDatasetBarChart />
      <MixedLineBarChart />
      <LiveChart />
      <RealtimeTimeseriesChart />
     </div>
  </div>
);

export default App;
