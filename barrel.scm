(barrel
 (name "scenario-edit")
 (version "0.0.1")
 (dependencies
  (from-git/build 'dbd.sqlite
                  (repo "https://github.com/mhayashi1120/Gauche-dbd-sqlite.git"))
  (from-git 'net.twitter
            (repo "https://github.com/mhayashi1120/Gauche-net-twitter.git")
            (branch #f))
  (from-git 'net.oauth
            (repo "https://github.com/mhayashi1120/Gauche-net-oauth.git")
            (branch #f))

  ))
