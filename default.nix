{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  overrides = self: super: let
    reflex-dom-contrib = pkgs.fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex-dom-contrib";
        rev = "b47f90c810c838009bf69e1f8dacdcd10fe8ffe3";
        sha256 = "0yvjnr9xfm0bg7b6q7ssdci43ca2ap3wvjhshv61dnpvh60ldsk9";
    };
    in
    {
      reflex-dom-contrib = self.callCabal2nix "reflex-dom-contrib" "${reflex-dom-contrib}" {};
    };
})
