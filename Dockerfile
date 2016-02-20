FROM lpenz/debian-squeeze-amd64

# install required packages:
ENV DEBIAN_FRONTEND noninteractive
RUN ["aptitude", "update"]
RUN ["aptitude", "install", "-R", "-y", "haskell-platform", "scons", "graphviz", "git", "inkscape", "imagemagick", "make", "inkscape", "ghostscript", "sudo" ]
RUN ["aptitude", "install", "-R", "-y", "texlive-full" ]

# setup sudo, used by abntex2 installation
RUN echo 'ALL ALL=NOPASSWD:ALL' > /etc/sudoers.d/all
RUN ["chmod", "0400", "/etc/sudoers.d/all"]

# setup common user
RUN ["adduser", "--disabled-password", "--gecos", "", "--quiet", "--uid", "1000", "user"]
USER user

# install iiufgs:
RUN ["git", "clone", "https://github.com/schnorr/iiufrgs.git", "/home/user/iiufrgs"]
RUN ["make","-C","/home/user/iiufrgs","install"]

# install abntex2:
RUN ["git", "clone", "https://github.com/abntex/abntex2.git", "/home/user/abntex2"]
RUN ["sed", "-i", "s@2012@2008@g", "/home/user/abntex2/Makefile"]
RUN ["sudo","make","-C","/home/user/abntex2","install"]
# (fool abntex2 about our tex version - it works anyway)

# final touches
ENV PATH /usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/home/user/fwlint/bin
ENV SCONSTOOLS /home/user/fwlint/sconstools
WORKDIR /home/user/fwlint
CMD ["./test"]

