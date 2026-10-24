# SPDX-License-Identifier: MIT
#
# The default package overlay: the elnix package under `pkgs.elnix`
# (`pkgs.elnix.elnix`). A consumer that lists this flake in `projects`
# holds it as `elnix/default`, which its package sets apply by default.
{ closure-lib, ... }:
{
  overlay = closure-lib.caisson.nixpkgs.mkPackagesOverlay ../../pkgs "elnix";
}
