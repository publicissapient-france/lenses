import fr.xebia.lenses.Main.{Address, Person}
import org.scalatest.{FlatSpec, Matchers}

class LensSpec extends FlatSpec with Matchers {

  case class Person(name: String, age: Int, address: Address)

  case class Address(number: Int, street: String)

  trait Lens[O, P] {
    self =>
    def get: O => P
    def set: (O, P) => O
    def update(p: P)(o: O): O = set(o, p)
    def and[_P](next: Lens[P, _P]): Lens[O, _P] = new Lens[O, _P] {
      override def get: (O) => _P = o => next.get(self.get(o))
      override def set: (O, _P) => O = {
        case (o, _p) => self.set(o, next.set(self.get(o), _p))
      }
    }
  }

  implicit val _personName = new Lens[Person, String] {
    override def get: (Person) => String = _.name
    override def set: (Person, String) => Person = {
      case (p, n) => p.copy(name = n)
    }
  }

  implicit val _personAddress = new Lens[Person, Address] {
    override def get: (Person) => Address = _.address
    override def set: (Person, Address) => Person = {
      case (p, a) => p.copy(address = a)
    }
  }

  implicit val _addressNumber = new Lens[Address, Int] {
    override def get: (Address) => Int = _.number
    override def set: (Address, Int) => Address = {
      case (a, n) => a.copy(number = n)
    }
  }

  "simple lens" should "modify direct value" in {
    val person = Person("Toto", 12, Address(10, "street"))
    val _personAddressNumber: Lens[Person, Int] = _personAddress.and(_addressNumber)
    val p = _personName.update("Mathieu")(person)
    val n = _personAddressNumber.get(p)
    val newP = _personAddressNumber.update(15)(person)

    p.toString shouldBe "Person(Mathieu,12,Address(10,street))"
    n shouldBe 10
    newP.toString shouldBe "Person(Toto,12,Address(15,street))"
  }

}
