package play.api.cache.redis.configuration

import org.specs2.mutable.Spec

/**
  * @author Karel Cemus
  */
class HostnameResolverSpec extends Spec {
  import HostnameResolver._

  "hostname is resolved to IP address" in {
    "localhost".resolvedIpAddress mustEqual "127.0.0.1"
  }

  "resolving IP address remains an address" in {
    "127.0.0.1".resolvedIpAddress mustEqual "127.0.0.1"
  }
}
