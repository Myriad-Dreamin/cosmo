package cosmo.eval

import scala.scalajs.js

import cosmo.{Scopes, DefId}

class ScopesTest extends munit.FunSuite:
  test("pushPop") {
    val scopes = new Scopes()
    scopes.set("a", DefId(0))
    assert(scopes.get("a") == Some(DefId(0)))
    scopes.push()
    assert(scopes.get("a") == Some(DefId(0)))
    scopes.set("a", DefId(1))
    assert(scopes.get("a") == Some(DefId(1)))
    scopes.pop()
    assert(scopes.get("a") == Some(DefId(0)))
  }
end ScopesTest
