FROM node:16-alpine3.15

WORKDIR "/app"

RUN npm install create-elm-app -g

RUN create-elm-app create-blockchain

RUN cd create-blockchain && ls -a

EXPOSE 3000
WORKDIR "/app/create-blockchain"
CMD ["elm-app", "start"]
