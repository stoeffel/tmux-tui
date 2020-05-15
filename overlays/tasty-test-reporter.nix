# Pulls in a Haskell library for reporting test results to stdout and junit-xml
# The library isn't available from `nixpkgs` yet, hence this overlay. As soon
# as we can get it from `nixpkgs` we should delete this overlay.  
#
# https://github.com/stoeffel/tasty-test-reporter
self: super:

{
  haskellPackages = super.haskellPackages.extend (haskellSelf: haskellSuper: {
    tasty-test-reporter = let
      src = builtins.fetchGit {
        url = "git@github.com:stoeffel/tasty-test-reporter";
        rev = "36782fd65647269a8fed086288b03857167b12cb";
      };
    in self.haskell.lib.dontCheck
    (self.haskellPackages.callCabal2nix "tasty-test-reporter" src { });
  });
}
