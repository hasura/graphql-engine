import React from 'react';
import { Button } from '@/new-components/Button';
import { GrHeroku } from 'react-icons/gr';

export function HerokuBanner() {
  return (
    <div className="border border-gray-300 border-l-4 border-l-indigo-600 shadow-md rounded bg-white p-md">
      <div className="flex p-sm items-center">
        <div className="ml-sm">
          <GrHeroku size={24} />
        </div>
        <div className="text-lg text-gray-700 ml-sm">
          Starting <b>November 28th, 2022,</b> free Heroku Dynos, free Heroku
          Postgres, and free Heroku Data for Redis will no longer be available.
        </div>
        <Button className="ml-auto">Know more</Button>
      </div>
    </div>
  );
}
