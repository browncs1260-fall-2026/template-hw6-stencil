FROM debian:stable

ARG username=cs1260_user
ARG tz=America/New_York
ENV TZ=${TZ}
ENV DEBIAN_FRONTEND="noninteractive" TZ="$tz"

# Sets up the container
RUN apt-get -y update && apt-get --fix-broken install -y \
    sudo bc dc vim neovim nano git git-doc psmisc tmux wget curl gpg zsh software-properties-common && \
    # Adds a user and creates a home directory
    useradd -ms /usr/bin/zsh $username && \
    # Sets up the user as a sudoer
    echo " ${username} ALL=(ALL) NOPASSWD: ALL" >>/etc/sudoers

USER $username
WORKDIR /home/$username
SHELL ["/bin/zsh", "-c"]

# C/C++ Tools and OCaml
RUN sudo apt-get -y update && sudo apt-get install -y build-essential nasm opam m4 python3 clang clang-format gdb

# Sets up OCaml
RUN opam init -a --disable-sandboxing && \
    eval $(opam env) && \
    opam update -y && \
    opam switch create 5.0.0 && \
    eval $(opam env --switch=5.0.0) && \
    # The janestreet-bleeding repo has packages that are not yet merged into the official opam repo,
    # which is required for OCaml 5.0.0 compatability. Specifically shexp requires posixat 0.16.0+.
    # For some reason official repo versions cause bugs and issues.
    opam repo add janestreet-bleeding https://ocaml.janestreet.com/opam-repository && \
    opam install -y core core_unix menhir ppx_blob ppx_deriving ppx_inline_test ppx_let shexp yojson utop ounit2 ocaml-lsp-server ocamlformat domainslib ppx_import odoc atdgen printbox printbox-text && \
    opam pin add -y ocamlformat 0.26.0
