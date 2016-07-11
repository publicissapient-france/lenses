package fr.xebia.lenses;

import lombok.Value;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class LensTest {

    @Value
    private class Person {
        public final String name;
        public final Integer age;
        public final Address address;
    }

    @Value
    private class Address {
        public final Integer number;
        public final String street;
    }

    interface Lens<O, P> {
        P get(O instance);
        O set(O instance, P value);
        default O update(P value, O instance) {
            return set(instance, value);
        }
        default <_P> Lens<O, _P> and(Lens<P, _P> next) {
            return new Lens<O, _P>() {
                @Override
                public _P get(O instance) {
                    return next.get(Lens.this.get(instance));
                }
                @Override
                public O set(O instance, _P value) {
                    return Lens.this.set(instance, next.set(Lens.this.get(instance), value));
                }
            };
        }
    }

    private Lens<Person, String> _personName = new Lens<Person, String>() {
        @Override
        public String get(Person person) {
            return person.name;
        }

        @Override
        public Person set(Person person, String name) {
            return new Person(name, person.age, person.address);
        }
    };

    private Lens<Person, Address> _personAddress = new Lens<Person, Address>() {
        @Override
        public Address get(Person person) {
            return person.address;
        }

        @Override
        public Person set(Person person, Address address) {
            return new Person(person.name, person.age, address);
        }
    };

    private Lens<Address, Integer> _addressNumber = new Lens<Address, Integer>() {
        @Override
        public Integer get(Address address) {
            return address.number;
        }

        @Override
        public Address set(Address address, Integer number) {
            return new Address(number, address.street);
        }
    };

    @Test
    public void should_modify_direct_value() {
        Person person = new Person("Toto", 12, new Address(10, "street"));
        Lens<Person, Integer> _personAddressNumber = _personAddress.and(_addressNumber);
        Person p = _personName.update("Mathieu", person);
        Integer n = _personAddressNumber.get(p);
        Person newP = _personAddressNumber.update(15, person);

        assertThat(p.toString(), equalTo("LensTest.Person(name=Mathieu, age=12, address=LensTest.Address(number=10, street=street))"));
        assertThat(n, equalTo(10));
        assertThat(newP.toString(), equalTo("LensTest.Person(name=Toto, age=12, address=LensTest.Address(number=15, street=street))"));
    }
}
