# SPDX-License-Identifier: MIT
#
# `builtins.derivation': the lazy set around `derivationStrict'.  Every
# output gets a set of its own, made from the attributes the derivation
# was called with plus one attribute per output, `all' and `drvAttrs';
# the derivation itself is the set of its first output.
drvAttrs@{
  outputs ? [ "out" ],
  ...
}:
let
  strict = derivationStrict drvAttrs;
  outputSet = outputName: {
    name = outputName;
    value = common // {
      outPath = builtins.getAttr outputName strict;
      drvPath = strict.drvPath;
      type = "derivation";
      inherit outputName;
    };
  };
  perOutput = map outputSet outputs;
  common =
    drvAttrs
    // builtins.listToAttrs perOutput
    // {
      all = map (o: o.value) perOutput;
      inherit drvAttrs;
    };
in
(builtins.head perOutput).value
