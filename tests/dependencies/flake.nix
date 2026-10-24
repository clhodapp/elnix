# SPDX-License-Identifier: MIT
{
  inputs.caisson.url = "github:nix-caisson/caisson";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
  # Same channel as the main flake, so dev-partition evals agree with the
  # exported pkgSet.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.treefmt-nix.url = "github:numtide/treefmt-nix";
  inputs.treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
  outputs = { ... }: { };
}
