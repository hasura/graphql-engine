FROM node:24-alpine
WORKDIR /root/app
COPY package.json .
RUN npm install --only=production
COPY . .
EXPOSE 8080
CMD npm start
#TODO: https://learnk8s.io/blog/smaller-docker-images