FROM denoland/deno

EXPOSE 3000

WORKDIR /app

ADD . /app

RUN deno cache main.ts

CMD ["run", "--allow-net", "main.ts"]