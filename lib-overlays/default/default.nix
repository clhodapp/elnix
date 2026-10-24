# SPDX-License-Identifier: MIT
{ ... }:
{

  overlay = final: prev: {
    elnix = prev.elnix or { };
  };

}
