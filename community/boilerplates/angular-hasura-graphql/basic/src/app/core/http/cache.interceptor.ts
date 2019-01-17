import { Injectable } from '@angular/core';
import { HttpEvent, HttpInterceptor, HttpHandler, HttpRequest, HttpResponse } from '@angular/common/http';
import { Observable, Subscriber } from 'rxjs';

import { HttpCacheService } from './http-cache.service';

/**
 * Caches HTTP requests.
 * Use ExtendedHttpClient fluent API to configure caching for each request.
 */
@Injectable()
export class CacheInterceptor implements HttpInterceptor {
  private forceUpdate = false;

  constructor(private httpCacheService: HttpCacheService) {}

  /**
   * Configures interceptor options
   * @param {{update: boolean}} options If update option is enabled, forces request to be made and updates cache entry.
   * @return {CacheInterceptor} The configured instance.
   */
  configure(options?: { update?: boolean } | null): CacheInterceptor {
    const instance = new CacheInterceptor(this.httpCacheService);
    if (options && options.update) {
      instance.forceUpdate = true;
    }
    return instance;
  }

  intercept(request: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
    if (request.method !== 'GET') {
      return next.handle(request);
    }

    return new Observable((subscriber: Subscriber<HttpEvent<any>>) => {
      const cachedData = this.forceUpdate ? null : this.httpCacheService.getCacheData(request.urlWithParams);
      if (cachedData !== null) {
        // Create new response to avoid side-effects
        subscriber.next(new HttpResponse(cachedData as Object));
        subscriber.complete();
      } else {
        next.handle(request).subscribe(
          event => {
            if (event instanceof HttpResponse) {
              this.httpCacheService.setCacheData(request.urlWithParams, event);
            }
            subscriber.next(event);
          },
          error => subscriber.error(error),
          () => subscriber.complete()
        );
      }
    });
  }
}
