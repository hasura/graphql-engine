import { rest } from 'msw';
import Endpoints from '../../../Endpoints';

export const eeLicenseInfo = {
  active: rest.get(Endpoints.entitlement, async (req, res, ctx) => {
    return res(
      ctx.status(200),
      ctx.json({
        status: 'active',
        type: 'trial',
        expiry_at: new Date(new Date().getTime() + 100000000),
        grace_at: new Date(),
      })
    );
  }),
  expired: rest.get(Endpoints.entitlement, async (req, res, ctx) => {
    return res(
      ctx.status(200),
      ctx.json({
        status: 'expired',
        type: 'trial',
        expiry_at: new Date(new Date().getTime() - 100000000),
        grace_at: new Date(),
      })
    );
  }),
  expiredWithoutGrace: rest.get(
    Endpoints.entitlement,
    async (req, res, ctx) => {
      return res(
        ctx.status(200),
        ctx.json({
          status: 'expired',
          type: 'trial',
          expiry_at: new Date(new Date().getTime() - 100000000),
        })
      );
    }
  ),
  expiredAfterGrace: rest.get(Endpoints.entitlement, async (req, res, ctx) => {
    return res(
      ctx.status(200),
      ctx.json({
        status: 'expired',
        type: 'trial',
        expiry_at: new Date(new Date().getTime() - 1000000000),
        grace_at: new Date(new Date().getTime() - 2000000000),
      })
    );
  }),
  deactivated: rest.get(Endpoints.entitlement, async (req, res, ctx) => {
    return res(
      ctx.status(200),
      ctx.json({
        status: 'deactivated',
        type: 'trial',
        expiry_at: new Date(),
        grace_at: new Date(),
      })
    );
  }),
  none: rest.get(Endpoints.entitlement, async (req, res, ctx) => {
    return res(
      ctx.status(200),
      ctx.json({
        status: 'none',
        type: 'trial',
        expiry_at: new Date(),
        grace_at: new Date(),
      })
    );
  }),
  noneOnce: rest.get(Endpoints.entitlement, async (req, res, ctx) => {
    return res.once(
      ctx.status(200),
      ctx.json({
        status: 'none',
        type: 'trial',
        expiry_at: new Date(),
        grace_at: new Date(),
      })
    );
  }),
};
