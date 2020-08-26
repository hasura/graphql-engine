// import React, { createContext, useContext, useState } from 'react';
// import { DataSourcesAPI, Driver } from '.';
// import { services } from './services';

// const DataSourceContext = createContext<DataSourcesAPI | undefined>(undefined);
// const DataSourceDriverContext = createContext<(driver: Driver) => void>(
//   () => {}
// );

// export const useDataSource = () => {
//   const context = useContext(DataSourceContext);

//   if (context === undefined) {
//     throw new Error(
//       `useDataSource can't be used outside of the context provider`
//     );
//   }

//   return context;
// };

// export const DataSourceProvider: React.FC = ({ children }) => {
//   const [driver, setDriver] = useState<Driver>('postgres');

//   return (
//     <DataSourceDriverContext.Provider value={setDriver}>
//       <DataSourceContext.Provider value={services[driver]}>
//         {children}
//       </DataSourceContext.Provider>
//     </DataSourceDriverContext.Provider>
//   );
// };
