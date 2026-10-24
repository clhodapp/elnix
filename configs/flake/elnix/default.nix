# SPDX-License-Identifier: MIT
{ ... }:
{
  config,
  inputs,
  lib,
  ...
}:
{

  imports = [ inputs.flake-parts.flakeModules.partitions ];

  debug = false;
  caisson = {
    libOverlays.exported = libOverlays: {
      inherit (libOverlays) default;
    };

    # The elnix package is the `default` entry of this flake's package
    # overlay registry (pkg-overlays/): the package set applies it by
    # default, and the flake exports it as `pkgOverlays` and as the plain
    # `overlays.default`.
    nixpkgs = {
      packages.export.enabled = true;
    };
  };

  # `nix run github:clhodapp/elnix -- eval -E '1 + 1'` reaches the
  # command line.
  perSystem =
    { config, ... }:
    {
      packages.default = config.packages.elnix;
    };

  partitionedAttrs.checks = "checks";
  partitionedAttrs.formatter = "formatter";

  partitions.formatter = {
    extraInputs = (lib.caisson.pins.flake-compat ../../../tests/dependencies).sources;
    module =
      { inputs, ... }:
      {
        imports = [ inputs.treefmt-nix.flakeModule ];
        perSystem.treefmt.programs.nixfmt.enable = true;
      };
  };

  partitions.checks = {
    extraInputs = (lib.caisson.pins.flake-compat ../../../tests/dependencies).sources;
    module =
      { inputs, self, ... }:
      {
        imports = [ inputs.treefmt-nix.flakeModule ];
        perSystem =
          { pkgs, system, ... }:
          let
            elnix = self.packages.${system}.elnix;
          in
          {
            checks = {
              # The package build runs the ERT tests.
              inherit elnix;

              # A derivation path computed by the installed command line
              # against the one Nix computes for the same expression.
              drv-path =
                let
                  expr = ''derivation { name = "x"; system = "s"; builder = "b"; }'';
                  want =
                    (derivation {
                      name = "x";
                      system = "s";
                      builder = "b";
                    }).drvPath;
                in
                pkgs.runCommand "elnix-drv-path" { nativeBuildInputs = [ elnix ]; } ''
                  export HOME=$TMPDIR ELNIX_CACHE_DIR=$TMPDIR/cache
                  got=$(elnix drv-path -E '${expr}')
                  [ "$got" = "${builtins.unsafeDiscardStringContext want}" ] \
                    || { echo "elnix: $got, nix: ${builtins.unsafeDiscardStringContext want}"; exit 1; }
                  touch $out
                '';
            };
            treefmt.programs.nixfmt.enable = true;
          };
      };
  };

}
