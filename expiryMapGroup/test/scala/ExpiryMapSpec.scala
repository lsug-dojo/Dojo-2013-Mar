import java.util.Date
import org.specs2.mutable.Specification

class ExpiryMapSpec extends Specification {

  "Expiry map" should {

    implicit def time = new Date().getTime

    "get entry" in {
      ExpiryMap(60000, ("key" -> "value")).get("key") must beSome( "value" )
    }

    "add a new entry" in {
      val map = ExpiryMap(60000) + ("key" -> 1)
      map.get("key") mustEqual 1
    }

  }

}
