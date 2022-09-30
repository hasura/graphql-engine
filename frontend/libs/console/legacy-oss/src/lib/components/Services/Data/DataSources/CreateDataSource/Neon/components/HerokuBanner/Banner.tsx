import React from 'react';
import { Button } from '@/new-components/Button';
import { GrHeroku } from 'react-icons/gr';

export function HerokuBanner() {
  return (
    <div className="flex items-center justify-between border border-gray-300 border-l-4 border-l-[#430098] shadow-md rounded bg-white p-md">
      <div className="flex items-center">
        <div className="ml-sm">
          <GrHeroku size={24} color="#430098" />
        </div>
        <div className="text-lg text-gray-700 ml-sm">
          Starting <b>November 28th, 2022,</b> free Heroku Dynos, free Heroku
          Postgres, and free Heroku Data for Redis will no longer be available.
        </div>
      </div>
      <a
        href="https://help.heroku.com/RSBRUH58/removal-of-heroku-free-product-plans-faq"
        className="no-underline"
        target="_blank"
        rel="noopener noreferrer"
      >
        <Button className="ml-auto">Know more</Button>
      </a>
    </div>
  );
}
