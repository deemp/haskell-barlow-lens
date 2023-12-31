{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs:
    let makeFlake = inputs.flakes.makeFlake; in
    makeFlake {
      inputs = {
        inherit (inputs.flakes.all)
          nixpkgs lima formatter codium drv-tools devshell
          flakes-tools workflows haskell-tools;
      };
      perSystem = { inputs, system }:
        let
          # We're going to make some dev tools for our Haskell package
          # The NixOS wiki has more info - https://nixos.wiki/wiki/Haskell

          # --- Imports ---

          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit (inputs.codium.lib.${system}) extensions extensionsCommon settingsNix settingsCommonNix writeSettingsJSON mkCodium;
          inherit (inputs.devshell.lib.${system}) mkCommands mkRunCommands mkShell;
          inherit (inputs.drv-tools.lib.${system}) mkBin withAttrs withMan withDescription mkShellApps man getExe;
          inherit (inputs.flakes-tools.lib.${system}) mkFlakesTools;
          inherit (inputs.haskell-tools.lib.${system}) toolsGHC;
          inherit (inputs.workflows.lib.${system}) writeWorkflow nixCI run steps;

          # --- Parameters ---

          # The desired GHC version
          ghcVersion = "928";

          # The name of a package
          barlow-lens = "barlow-lens";

          # --- Override ---

          # Here's our override
          # Haskell overrides are described here: https://nixos.org/manual/nixpkgs/unstable/#haskell
          override = {
            overrides = self: super: {
              ${barlow-lens} = super.callCabal2nix barlow-lens ./. { };
              lima = inputs.lima.outputs.packages.${system}.default;
              make-docs = super.callCabal2nix "make-docs" ./make-docs { };
            };
          };

          # --- Haskell tools ---

          # We call a helper function that will give us tools for Haskell
          inherit (toolsGHC {
            version = ghcVersion;
            inherit override;

            # If we work on multiple packages, we need to supply all of them
            # so that their dependencies can be correctrly filtered.

            # Suppose we develop packages A and B, where B is in dependencies of A.
            # GHC will be given dependencies of both A and B.
            # However, we don't want B to be in the list of dependencies of GHC
            # because build of GHC may fail due to errors in B.
            packages = ps: [ ps.${barlow-lens} ];
          })
            hls cabal fourmolu implicit-hie justStaticExecutable
            ghcid callCabal2nix haskellPackages hpack;

          # --- Tools ---

          # We list the tools that we'd like to use
          tools = [
            ghcid
            hpack
            implicit-hie
            fourmolu
            cabal
            hls
          ];

          # --- Packages ---

          packages =
            let
              packages1 = mkShellApps {
                writeDocs = {
                  text = getExe haskellPackages.make-docs;
                  description = "Write docs";
                };
              };
              packages2 = {
                # --- Haskell package ---

                # This is a static executable with given runtime dependencies.
                # In this case, its name is the same as the package name.
                default = haskellPackages.${barlow-lens};

                # --- IDE ---

                codium = mkCodium {
                  extensions = extensionsCommon // { inherit (extensions) haskell; };
                };

                # a script to write `.vscode/settings.json`
                writeSettings = writeSettingsJSON (settingsCommonNix // { inherit (settingsNix) haskell; });

                # --- Flakes ---

                # Scripts that can be used in CI
                inherit (mkFlakesTools { dirs = [ "." ]; root = ./.; }) updateLocks format saveFlakes pushToCachix;

                # --- GH Actions

                # A script to write GitHub Actions workflow file into `.github/ci.yaml`
                writeWorkflows = writeWorkflow "ci" (nixCI {
                  jobArgs = {
                    cacheNixArgs = {
                      linuxGCEnabled = true;
                      linuxMaxStoreSize = 4500000000;
                      macosGCEnabled = true;
                      macosMaxStoreSize = 4500000000;
                    };
                    doCommit = false;
                    steps = dir: [
                      {
                        name = "Write docs";
                        run = run.nixScript { name = packages1.writeDocs.pname; };
                      }
                      (steps.commit { messages = [ (steps.updateLocks { }).name (steps.format { }).name "Write docs" ]; })
                      {
                        name = "Build package";
                        run = run.nixScript { name = "default"; doRun = false; };
                      }
                    ];
                  };
                });
              };
            in
            packages1 // packages2;

          # --- Devshells ---

          devShells = {
            default = mkShell {
              packages = tools;
              # sometimes necessary for programs that work with files
              bash.extra = "export LANG=C.utf8";
              commands =
                mkCommands "tools" tools
                ++ mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; }
                ++ mkRunCommands "infra" { inherit (packages) writeWorkflows updateLocks pushToCachix; }
                ++ mkRunCommands "docs" { inherit (packages) writeDocs; };
            };
          };
        in
        {
          inherit packages devShells;
          formatter = inputs.formatter.${system};
        };
    };

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}
