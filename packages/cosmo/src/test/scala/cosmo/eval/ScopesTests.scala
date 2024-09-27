package cosmo.eval

import scala.scalajs.js

import cosmo.{Source, Scopes, DefInfo, Env, Cosmo}

class ScopesTest extends munit.FunSuite:
  test("pushPop") {
    val scopes = new Scopes()
    val env = new Env(Source.empty, new Cosmo())
    val a = DefInfo.just(0, env)
    val b = DefInfo.just(1, env)
    scopes.set("a", a)
    assert(scopes.get("a") == Some(a))
    scopes.withScope {
      assert(scopes.get("a") == Some(a))
      scopes.set("a", b)
      assert(scopes.get("a") == Some(b))
    }
    assert(scopes.get("a") == Some(a))
  }
end ScopesTest
