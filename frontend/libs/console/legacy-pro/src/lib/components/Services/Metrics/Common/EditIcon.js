import React from 'react';

export const EditIcon = ({ className, onClick }) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    version="1.1"
    viewBox="0 0 48 48"
    height="24"
    width="24"
    onClick={onClick}
    className={className}
  >
    <g transform="matrix(2,0,0,2,0,0)">
      <path
        d="M 22.63,14.87L15,22.5l-3.75,0.75L12,19.5l7.63-7.63c0.825-0.826,2.163-0.827,2.99-0.002 c0.001,0.001,0.002,0.002,0.002,0.002l0.008,0.008C23.428,12.699,23.405,14.027,22.63,14.87z "
        stroke="#505050"
        fill="none"
        strokeWidth="2.5"
        strokeLinecap="round"
        strokeLinejoin="round"
      />
      <path
        d="M 8.25,20.25h-6 c-0.828,0-1.5-0.672-1.5-1.5V2.25c0-0.828,0.672-1.5,1.5-1.5h10.629c0.398,0,0.779,0.158,1.06,0.439l2.872,2.872 c0.281,0.281,0.439,0.662,0.439,1.06V8.25"
        stroke="#505050"
        fill="none"
        strokeWidth="2.5"
        strokeLinecap="round"
        strokeLinejoin="round"
      />
    </g>
  </svg>
);
