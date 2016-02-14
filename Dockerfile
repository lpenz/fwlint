FROM lpenz/debian-squeeze-amd64
RUN ["aptitude", "update"]
RUN ["aptitude", "install", "-R", "-y", "haskell-platform", "scons" ]
RUN adduser --disabled-password --gecos "" --quiet fwlint
USER fwlint
WORKDIR /home/fwlint/fwlint
