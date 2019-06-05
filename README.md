### 

#### nix-shell
''' nix-shell --packages 'haskellPackages.ghcWithHoogle (pkgs: [ pkgs.text pkgs.safe ])' haskellPackages.ghcid '''
''' ghcid --command='ghci <filename>.hs' '''
''' hoogle 'Text -> (Text, Text)' ''' 
''' hoogle server '''
