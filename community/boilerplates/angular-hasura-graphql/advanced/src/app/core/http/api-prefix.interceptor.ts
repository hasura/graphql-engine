import { Injectable } from '@angular/core';
import {
  HttpEvent,
  HttpInterceptor,
  HttpHandler,
  HttpRequest
} from '@angular/common/http';
import { Observable } from 'rxjs';

import { environment } from '@env/environment';

/**
 * Prefixes all requests with `environment.serverUrl`.
 */
@Injectable()
export class ApiPrefixInterceptor implements HttpInterceptor {
  intercept(
    request: HttpRequest<any>,
    next: HttpHandler
  ): Observable<HttpEvent<any>> {
    if (!/^(http|https):/i.test(request.url)) {
      request = request.clone({ url: environment.serverUrl + request.url });
    }
    return next.handle(request);
  }
}
