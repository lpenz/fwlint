FROM lpenz/debian-sarge-i386
RUN ["aptitude", "install", "-R", "-y", "ghc6", "scons" ]
RUN adduser --disabled-password --gecos "" --quiet fwlint
USER fwlint
WORKDIR /home/fwlint
