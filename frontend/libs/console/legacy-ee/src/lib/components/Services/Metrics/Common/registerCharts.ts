import {
  BarElement,
  CategoryScale,
  Chart,
  Filler,
  Legend,
  LinearScale,
  LineElement,
  PointElement,
  Tooltip,
} from 'chart.js';

// Chart.js v3+ is tree-shakeable: only the pieces registered here are bundled.
// Import this module (for its side effect) from every chart component.
Chart.register(
  BarElement,
  CategoryScale,
  Filler,
  Legend,
  LinearScale,
  LineElement,
  PointElement,
  Tooltip,
);

// Chart.js v2 (via react-chartjs-2 v2) put the legend at the bottom; keep that.
Chart.defaults.plugins.legend.position = 'bottom';
