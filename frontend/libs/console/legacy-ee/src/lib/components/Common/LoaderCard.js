import React from 'react';

const LoaderCard = ({ width = '200px', height = '200px' }) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      // xmlns:xlink="http://www.w3.org/1999/xlink"
      style={{
        margin: 'auto',
        background: 'none',
        display: 'block',
        shapeRendering: 'auto',
      }}
      width={width}
      height={height}
      viewBox="0 0 100 100"
      preserveAspectRatio="xMidYMid"
    >
      <g transform="rotate(180 50 50)">
        <rect
          x="10.666666666666668"
          y="12.5"
          width="12"
          height="40"
          fill="#dddddd"
        >
          <animate
            attributeName="height"
            calcMode="spline"
            values="50;75;10;50"
            times="0;0.33;0.66;1"
            dur="1.5384615384615383s"
            keySplines="0.5 0 0.5 1;0.5 0 0.5 1;0.5 0 0.5 1"
            repeatCount="indefinite"
            begin="0s"
          />
        </rect>
        <rect
          x="27.333333333333336"
          y="12.5"
          width="12"
          height="40"
          fill="#e4e4e4"
        >
          <animate
            attributeName="height"
            calcMode="spline"
            values="50;75;10;50"
            times="0;0.33;0.66;1"
            dur="1.5384615384615383s"
            keySplines="0.5 0 0.5 1;0.5 0 0.5 1;0.5 0 0.5 1"
            repeatCount="indefinite"
            begin="-1.2307692307692308s"
          />
        </rect>
        <rect x="44" y="12.5" width="12" height="40" fill="#ebebeb">
          <animate
            attributeName="height"
            calcMode="spline"
            values="50;75;10;50"
            times="0;0.33;0.66;1"
            dur="1.5384615384615383s"
            keySplines="0.5 0 0.5 1;0.5 0 0.5 1;0.5 0 0.5 1"
            repeatCount="indefinite"
            begin="-0.923076923076923s"
          />
        </rect>
        <rect
          x="60.66666666666667"
          y="12.5"
          width="12"
          height="40"
          fill="#f1f1f1"
        >
          <animate
            attributeName="height"
            calcMode="spline"
            values="50;75;10;50"
            times="0;0.33;0.66;1"
            dur="1.5384615384615383s"
            keySplines="0.5 0 0.5 1;0.5 0 0.5 1;0.5 0 0.5 1"
            repeatCount="indefinite"
            begin="-0.6153846153846154s"
          />
        </rect>
        <rect
          x="77.33333333333333"
          y="12.5"
          width="12"
          height="40"
          fill="#f8f8f8"
        >
          <animate
            attributeName="height"
            calcMode="spline"
            values="50;75;10;50"
            times="0;0.33;0.66;1"
            dur="1.5384615384615383s"
            keySplines="0.5 0 0.5 1;0.5 0 0.5 1;0.5 0 0.5 1"
            repeatCount="indefinite"
            begin="-0.3076923076923077s"
          />
        </rect>
      </g>
    </svg>
  );
};

export default LoaderCard;
