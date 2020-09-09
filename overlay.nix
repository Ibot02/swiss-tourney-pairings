self: super:
  let
    ghcjsVersion = "ghcjs86";
    servant-src = super.fetchFromGitHub {
      owner = "haskell-servant";
      repo = "servant";
      rev = "e3e5d2b23057c2c3409e5e210b613527baf3b77d";
      sha256 = "0n9xn2f61mprnvn9838zbl4dv2ynnl0kxxrcpf5c0igdrks8pqws";
    };
    pkgs = super.pkgs;
    haskell-ghcjs-packages = super.haskell.packages.${ghcjsVersion}.override (old: {
      overrides = super.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: let
        servant-package = name :
          pkgs.haskell.lib.doJailbreak (super.callCabal2nix name (servant-src + "/${name}") {});
        in {
        servant-client-ghcjs = servant-package "servant-client-ghcjs";
      });
    });
  in
    {
      haskell = super.haskell // {
        packages = super.haskell.packages // {
          ${ghcjsVersion} = haskell-ghcjs-packages;
        };
      };
    }
