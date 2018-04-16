FROM mono

RUN curl -sL https://deb.nodesource.com/setup_8.x | bash -
RUN apt-get update && apt-get install -y nodejs liblmdb0
RUN /usr/bin/npm config set @zen:registry https://www.myget.org/F/zenprotocol/npm/
RUN /usr/bin/npm install @zen/zen-node@0.1.40 -g

CMD ["zen-node"]


