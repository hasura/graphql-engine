import React from 'react';
import { KnowMoreLink } from '@/new-components/KnowMoreLink';
import { GrHeroku } from 'react-icons/gr';

export function HerokuBanner() {
  return (
    <div className="flex items-center">
      <GrHeroku size={15} className="mr-xs" color="#430098" />
      <div className="text-sm text-gray-700">
        Heroku free database integration support has been deprecated.{' '}
        <KnowMoreLink href="https://hasura.io/docs/latest/databases/connect-db/cloud-databases/heroku/" />
      </div>
    </div>
  );
}
