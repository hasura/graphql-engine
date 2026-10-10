import { Navigate, Route } from 'react-router';
import { AllowListDetail } from './components/AllowListDetail';

const getAllowListRoutes = () => {
  return (
    <Route path="allow-list">
      <Route index element={<Navigate to="detail" replace />} />
      <Route path="detail" element={<AllowListDetail />} />
      <Route path="detail/:name" element={<AllowListDetail />} />
      <Route path="detail/:name/:section" element={<AllowListDetail />} />
    </Route>
  );
};

export default getAllowListRoutes;
