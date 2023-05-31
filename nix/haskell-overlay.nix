{ sources ? import ./sources.nix, pkgs }:
self: super: {
  hoff = self.callPackage ../hoff.nix { };
}
