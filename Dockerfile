FROM nginx:1.13.3

COPY _build/conf/nginx.conf /etc/nginx/nginx.conf
COPY _build/conf/mime.types /etc/nginx/mime.types

COPY _build /usr/share/nginx/html
