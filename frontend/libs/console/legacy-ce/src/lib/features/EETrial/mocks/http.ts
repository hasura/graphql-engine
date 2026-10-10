import { http, HttpResponse } from 'msw';
import Endpoints from '../../../Endpoints';

export const eeLicenseInfo = {
  active: http.get(Endpoints.entitlement, async () => {
    return HttpResponse.json(
      {
        status: 'active',
        type: 'trial',
        expiry_at: new Date(new Date().getTime() + 100000000),
        grace_at: new Date(),
      },
      { status: 200 },
    );
  }),
  expired: http.get(Endpoints.entitlement, async () => {
    return HttpResponse.json(
      {
        status: 'expired',
        type: 'trial',
        expiry_at: new Date(new Date().getTime() - 100000000),
        grace_at: new Date(),
      },
      { status: 200 },
    );
  }),
  expiredWithoutGrace: http.get(Endpoints.entitlement, async () => {
    return HttpResponse.json(
      {
        status: 'expired',
        type: 'trial',
        expiry_at: new Date(new Date().getTime() - 100000000),
      },
      { status: 200 },
    );
  }),
  expiredAfterGrace: http.get(Endpoints.entitlement, async () => {
    return HttpResponse.json(
      {
        status: 'expired',
        type: 'trial',
        expiry_at: new Date(new Date().getTime() - 1000000000),
        grace_at: new Date(new Date().getTime() - 2000000000),
      },
      { status: 200 },
    );
  }),
  deactivated: http.get(Endpoints.entitlement, async () => {
    return HttpResponse.json(
      {
        status: 'deactivated',
        type: 'trial',
        expiry_at: new Date(),
        grace_at: new Date(),
      },
      { status: 200 },
    );
  }),
  none: http.get(Endpoints.entitlement, async () => {
    return HttpResponse.json(
      {
        status: 'none',
        type: 'trial',
        expiry_at: new Date(),
        grace_at: new Date(),
      },
      { status: 200 },
    );
  }),
  noneOnce: http.get(
    Endpoints.entitlement,
    async () => {
      return HttpResponse.json(
        {
          status: 'none',
          type: 'trial',
          expiry_at: new Date(),
          grace_at: new Date(),
        },
        { status: 200 },
      );
    },
    { once: true },
  ),
};
