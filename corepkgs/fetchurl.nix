# SPDX-License-Identifier: MIT
#
# `<nix/fetchurl.nix>': a fixed-output derivation the builtin:fetchurl
# builder realises.  The nixpkgs bootstrap imports it, so the attributes
# below are the ones whose values reach derivation hashes: an SRI `hash'
# is used as given, otherwise the strongest legacy hash that is set.
{ system ? ""
, url
, hash ? ""
, md5 ? ""
, sha1 ? ""
, sha256 ? ""
, sha512 ? ""
, outputHash ? (if hash != "" then hash
                else if sha512 != "" then sha512
                else if sha1 != "" then sha1
                else if md5 != "" then md5
                else sha256)
, outputHashAlgo ? (if hash != "" then ""
                    else if sha512 != "" then "sha512"
                    else if sha1 != "" then "sha1"
                    else if md5 != "" then "md5"
                    else "sha256")
, executable ? false
, unpack ? false
, name ? baseNameOf (toString url)
, impure ? false
}:
let
  hashAttrs =
    if impure then { __impure = true; } else { inherit outputHashAlgo outputHash; };
in
derivation ({
  builder = "builtin:fetchurl";
  outputHashMode = if unpack || executable then "recursive" else "flat";
  inherit name url executable unpack;
  system = "builtin";
  preferLocalBuild = true;
  impureEnvVars = [ "http_proxy" "https_proxy" "ftp_proxy" "all_proxy" "no_proxy" ];
  urls = [ url ];
} // hashAttrs)
