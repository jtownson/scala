import java.time.LocalDate

import com.softwaremill.quicklens._

object LensPerson {
  case class Person(_name: Name, _born: Born, _address: Address)

  case class Name(_foreNames: String /*Space separated*/ , _surName: String)

  // Value of java.time.LocalDate.toEpochDay
  type EpochDay = Long

  case class Born(_bornAt: Address, _bornOn: EpochDay)

  case class Address(_street: String, _houseNumber: Int,
    _place: String /*Village / city*/ , _country: String)

  // Valid values of Gregorian are those for which 'java.time.LocalDate.of'
  // returns a valid LocalDate.
  //case class Gregorian(_year: Int, _month: Int, _dayOfMonth: Int)

  private val updateMonthTo: Int => EpochDay => EpochDay = month => epochDay =>
    LocalDate.ofEpochDay(epochDay).withMonth(month).toEpochDay

  // Implement these.
  val bornStreet: Born => String = _._bornAt._street

  val setCurrentStreet: String => Person => Person =
    street => person => modify(person)(_._address._street).setTo(street)

  val setBirthMonth: Int => Person => Person =
    month => person => modify(person)(_._born._bornOn).using(updateMonthTo(month))

  // Transform both birth and current street names.
  val renameStreets: (String => String) => Person => Person =
    fStr => person => modifyAll(person)(_._born._bornAt._street, _._address._street).using(fStr)
}
