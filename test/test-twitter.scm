(use srfi-98)
(use gauche.test)

(add-load-path "../gosh-modules/net-twitter" :relative)
(add-load-path "../gosh-modules/net-twitter/Gauche-net-oauth" :relative)

(use net.twitter)
(test-module 'net.twitter)

(use net.oauth)
(test-module 'net.oauth)

(define *twitter-cred*
  (make <twitter-cred>
    :consumer-key (get-environment-variable "TWITTER_API_KEY")
    :consumer-secret (get-environment-variable "TWITTER_API_KEY_SECRET")
    :access-token (get-environment-variable "TWITTER_ACCESS_TOKEN")
    :access-token-secret (get-environment-variable "TWITTER_ACCESS_TOKEN_SECRET")))

(twitter-update *twitter-cred* "Post from my application!")
