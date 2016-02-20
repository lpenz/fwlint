FROM lpenz/debian-squeeze-amd64
ENV DEBIAN_FRONTEND noninteractive
RUN ["aptitude", "update"]
RUN ["aptitude", "install", "-R", "-y", "haskell-platform", "scons", "graphviz", "git", "inkscape", "imagemagick", "make", "inkscape", "ghostscript", "texlive-full", "texlive-lang-portuguese" ]
RUN adduser --disabled-password --gecos "" --quiet --uid 1000 fwlint
USER fwlint
ENV PATH /usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/home/fwlint/fwlint/bin
RUN ["git", "clone", "https://github.com/schnorr/iiufrgs.git", "/home/fwlint/iiufrgs"]
RUN ["make","-C","/home/fwlint/iiufrgs","install"]
WORKDIR /home/fwlint/fwlint
