# SPDX-License-Identifier: MIT
#
# elnix: the Lisp files byte-compiled for the Emacs they ship with, the
# corepkgs, and bin/elnix bound to that Emacs.
{
  lib,
  stdenvNoCC,
  emacs-nox,
  makeWrapper,
}:
stdenvNoCC.mkDerivation {
  pname = "elnix";
  version = "0.1.0";

  src = lib.fileset.toSource {
    root = ../..;
    fileset = lib.fileset.unions [
      ../../lisp
      ../../corepkgs
      ../../bin
      ../../test
      ../../LICENSE
      ../../README.md
    ];
  };

  nativeBuildInputs = [ makeWrapper ];

  buildPhase = ''
    runHook preBuild
    ${emacs-nox}/bin/emacs --batch -Q -L lisp -f batch-byte-compile lisp/*.el
    runHook postBuild
  '';

  # The ERT tests run against the byte-compiled files.
  doCheck = true;
  checkPhase = ''
    runHook preCheck
    export HOME=$TMPDIR ELNIX_CACHE_DIR=$TMPDIR/elnix-cache
    for t in test/*-test.el; do
      ${emacs-nox}/bin/emacs --batch -Q -L lisp -l "$t" -f ert-run-tests-batch-and-exit
    done
    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall
    share=$out/share/emacs/site-lisp/elnix
    mkdir -p $share $out/bin
    cp -r lisp corepkgs $share/
    makeWrapper ${emacs-nox}/bin/emacs $out/bin/elnix \
      --add-flags "--batch -Q -L $share/lisp -l elnix-cli -f elnix-cli-main"
    runHook postInstall
  '';

  meta = {
    description = "Nix compiled to Emacs Lisp, with the derivations Nix computes";
    license = lib.licenses.mit;
    mainProgram = "elnix";
    platforms = lib.platforms.unix;
  };
}
