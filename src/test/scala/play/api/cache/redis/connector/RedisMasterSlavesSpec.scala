package play.api.cache.redis.connector

import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification
import org.specs2.specification.{AfterAll, BeforeAll}
import play.api.cache.redis.configuration.{RedisHost, RedisMasterSlaves}
import play.api.cache.redis.impl.{LazyInvocation, RedisRuntime}
import play.api.cache.redis.{Implicits, LogAndFailPolicy, RedisConnector, Shutdown, WithApplication}
import play.api.inject.ApplicationLifecycle

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.DurationInt

/**
  * To run the test, you need to run master on port 6379, slave on port 6479.
  */
class RedisMasterSlavesSpec(implicit ee: ExecutionEnv) extends Specification with BeforeAll with AfterAll with WithApplication {

  import Implicits._

  implicit private val lifecycle = application.injector.instanceOf[ApplicationLifecycle]

  implicit private val runtime = RedisRuntime("master-slaves", syncTimeout = 5.seconds, ExecutionContext.global, new LogAndFailPolicy, LazyInvocation)

  private val serializer = new AkkaSerializerImpl(system)

  private val masterSlavesInstance = RedisMasterSlaves(defaultCacheName, master = RedisHost(dockerIp, 6379), slaves = RedisHost(dockerIp, 6479) :: Nil, defaults)

  private val connector: RedisConnector = new RedisConnectorProvider(masterSlavesInstance, serializer).get

  val prefix = "master-slaves-test"

  "Redis master-slaves (separate)" should {

    "pong on ping" in new TestCase {
      connector.ping() must not(throwA[Throwable]).await
    }

    "miss on get" in new TestCase {
      connector.get[String](s"$prefix-$idx") must beNone.await
    }

    "hit after set" in new TestCase {
      connector.set(s"$prefix-$idx", "value") must beTrue.await
      connector.get[String](s"$prefix-$idx") must beSome[Any].await
      connector.get[String](s"$prefix-$idx") must beSome("value").await
    }

    "ignore set if not exists when already defined" in new TestCase {
      connector.set(s"$prefix-if-not-exists-when-exists", "previous") must beTrue.await
      connector.set(s"$prefix-if-not-exists-when-exists", "value", ifNotExists = true) must beFalse.await
      connector.get[String](s"$prefix-if-not-exists-when-exists") must beSome("previous").await
    }

    "perform set if not exists when undefined" in new TestCase {
      connector.get[String](s"$prefix-if-not-exists") must beNone.await
      connector.set(s"$prefix-if-not-exists", "value", ifNotExists = true) must beTrue.await
      connector.get[String](s"$prefix-if-not-exists") must beSome("value").await
      connector.set(s"$prefix-if-not-exists", "other", ifNotExists = true) must beFalse.await
      connector.get[String](s"$prefix-if-not-exists") must beSome("value").await
    }

    "perform set if not exists with expiration" in new TestCase {
      connector.get[String](s"$prefix-if-not-exists-with-expiration") must beNone.await
      connector.set(s"$prefix-if-not-exists-with-expiration", "value", 2.seconds, ifNotExists = true) must beTrue.await
      connector.get[String](s"$prefix-if-not-exists-with-expiration") must beSome("value").await
      // wait until the first duration expires
      Future.after(3) must not(throwA[Throwable]).awaitFor(4.seconds)
      connector.get[String](s"$prefix-if-not-exists-with-expiration") must beNone.await
    }
  }

  def beforeAll(): Unit = {
    // initialize the connector by flushing the database
    connector.matching(s"$prefix-*").flatMap(connector.remove).await
  }

  def afterAll(): Unit = {
    Shutdown.run
  }
}
