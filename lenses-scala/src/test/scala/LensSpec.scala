import org.scalatest.{FlatSpec, Matchers}

class LensSpec extends FlatSpec with Matchers {

  case class Street(number: Int, name: String)
  case class Town(postalCode: String, name: String)
  case class Address(street: Street, town: Town)
  case class Person(firstName: String, lastName: String, address: Address)

  def updateStreetNumber(person: Person, streetNumber: Int): Person = person.copy(
    address = person.address.copy(
      street = person.address.street.copy(
        number = streetNumber
      )
    )
  )

  "boiler plate function" should "update street number" in {
    val person = Person("John", "Doe", Address(Street(12, "Rue de Picpus"), Town("75012", "Paris")))

    val updatedPerson = updateStreetNumber(person, 45)

    updatedPerson.address.street.number should be(45)
  }

  def updateFirstName(person: Person, firstName: String) = person.copy(firstName = firstName)

  def updateAddress(person: Person, address: Address): Person = person.copy(address = address)

  def updateStreet(person: Person, street: Street): Person = person.copy(
    address = person.address.copy(
      street = street
    )
  )

  trait Lens[S, P] {
    self =>
    def get: S => P
    def set: (S, P) => S
    def update(p: P)(s: S): S = set(s, p)
    def and[_P](next: Lens[P, _P]): Lens[S, _P] = new Lens[S, _P] {
      override def get: (S) => _P = o => next.get(self.get(o))
      override def set: (S, _P) => S = {
        case (o, _p) => self.set(o, next.set(self.get(o), _p))
      }
    }
  }

  implicit val _personFirstName = new Lens[Person, String] {
    override def get: (Person) => String = _.firstName
    override def set: (Person, String) => Person = {
      case (p, n) => p.copy(firstName = n)
    }
  }

  implicit val _personAddress = new Lens[Person, Address] {
    override def get: (Person) => Address = _.address
    override def set: (Person, Address) => Person = {
      case (p, a) => p.copy(address = a)
    }
  }

  "lenses" should "update values" in {
    val person = Person("John", "Doe", Address(Street(12, "Rue de Picpus"), Town("75012", "Paris")))
    val p1 = _personFirstName.update("Joe")(person)

    _personFirstName.get(p1) should be("Joe")

    val p2 = _personAddress.update(Address(Street(20, "Rue de Montreuil"), Town("75011", "Paris")))(person)

    _personAddress.get(p2) should be(Address(Street(20, "Rue de Montreuil"), Town("75011", "Paris")))

    val p3 = _personAddress.update(Address(Street(20, "Rue de Montreuil"), Town("75011", "Paris")))(p1)

    _personAddress.get(p3) should be(Address(Street(20, "Rue de Montreuil"), Town("75011", "Paris")))
  }

  val _addressStreet = new Lens[Address, Street] {
    override def get: (Address) => Street = _.street

    override def set: (Address, Street) => Address = {
      case (a, s) => a.copy(street = s)
    }
  }

  val _streetNumber = new Lens[Street, Int] {
    override def get: (Street) => Int = _.number

    override def set: (Street, Int) => Street = {
      case (s, n) => s.copy(number = n)
    }
  }

  val _streetName = new Lens[Street, String] {
    override def get: (Street) => String = _.name

    override def set: (Street, String) => Street = {
      case (s, n) => s.copy(name = n)
    }
  }

  val _personStreetNumber: Lens[Person, Int] = _personAddress.and(_addressStreet).and(_streetNumber)
  val _personStreetName: Lens[Person, String] = _personAddress.and(_addressStreet).and(_streetName)

  "lenses composition" should "update street number" in {
    val person = Person("John", "Doe", Address(Street(12, "Rue de Picpus"), Town("75012", "Paris")))

    val updatedPerson = _personStreetNumber.update(45)(person)

    updatedPerson.address.street.number should be(45)
  }

}
