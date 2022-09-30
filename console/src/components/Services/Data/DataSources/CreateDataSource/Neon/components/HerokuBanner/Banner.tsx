import React from 'react';
import { Button } from '@/new-components/Button';
import { GrHeroku } from 'react-icons/gr';

export function HerokuBanner() {
  return (
    <div className="flex items-center justify-between border border-gray-300 border-l-4 border-l-[#430098] shadow-md rounded bg-white p-md">
      <div className="w-[70%] flex items-center">
        <div className="mr-2">
          <GrHeroku size={30} color="#430098" />
        </div>
        <div className="text-md text-gray-700">
          Starting <b>November 28th, 2022,</b> free Heroku Dynos, free Heroku
          Postgres, and free Heroku Data for Redis will no longer be available.
        </div>
      </div>
      <div>
        <a
          href="https://help.heroku.com/RSBRUH58/removal-of-heroku-free-product-plans-faq"
          className="hover:no-underline"
          target="_blank"
          rel="noopener noreferrer"
        >
          <Button className="ml-auto">Know more</Button>
        </a>
      </div>
    </div>
  );
}
