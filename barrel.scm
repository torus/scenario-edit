(barrel
 (name "scenario-edit")
 (version "0.0.1")
 (dependencies
  (from-git/build "dbd-sqlite"
                  (repo "https://github.com/mhayashi1120/Gauche-dbd-sqlite.git"))
  (from-git/build "net-twitter"
                  (repo "https://github.com/mhayashi1120/Gauche-net-twitter.git"))))
