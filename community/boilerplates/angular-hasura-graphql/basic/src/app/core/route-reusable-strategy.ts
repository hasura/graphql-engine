import { ActivatedRouteSnapshot, DetachedRouteHandle, RouteReuseStrategy } from '@angular/router';

/**
 * A route strategy allowing for explicit route reuse.
 * Used as a workaround for https://github.com/angular/angular/issues/18374
 * To reuse a given route, add `data: { reuse: true }` to the route definition.
 */
export class RouteReusableStrategy extends RouteReuseStrategy {
  public shouldDetach(route: ActivatedRouteSnapshot): boolean {
    return false;
  }

  public store(route: ActivatedRouteSnapshot, detachedTree: DetachedRouteHandle | null): void {}

  public shouldAttach(route: ActivatedRouteSnapshot): boolean {
    return false;
  }

  public retrieve(route: ActivatedRouteSnapshot): DetachedRouteHandle | null {
    return null;
  }

  public shouldReuseRoute(future: ActivatedRouteSnapshot, curr: ActivatedRouteSnapshot): boolean {
    return future.routeConfig === curr.routeConfig || future.data.reuse;
  }
}
