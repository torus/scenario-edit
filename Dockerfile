FROM torus/violet:latest

RUN apt-get install -y gdb
RUN curl -sL https://deb.nodesource.com/setup_14.x | bash -
RUN apt-get install -y nodejs
RUN npm install -g nodemon

RUN apt-get install -y jq

WORKDIR /code
CMD make run
