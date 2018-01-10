
import java.time.temporal.TemporalAdjusters.firstInMonth
import java.time.{DayOfWeek, LocalDate}

import Schedule.Schedule

case class Meetup(month: Int, year: Int) {

  def day(dayOfWeek: Int, schedule: Schedule): LocalDate = {

    val dim = daysInMonth(dayOfWeek)

    schedule match {
      case Schedule.First => dim(0)
      case Schedule.Second => dim(1)
      case Schedule.Third => dim(2)
      case Schedule.Fourth => dim(3)
      case Schedule.Last => dim.last
      case Schedule.Teenth => dim.filter(isTeenth).head
    }
  }

  private def daysInMonth(dayOfWeek: Int): Seq[LocalDate] = {
    new Iterator[LocalDate] {

      var current: LocalDate = LocalDate.of(year, month, 1).`with`(firstInMonth(DayOfWeek.of(dayOfWeek))).minusDays(7)

      override def hasNext: Boolean = current.plusDays(7).getMonthValue == month

      override def next(): LocalDate = {
        current = current.plusDays(7)
        current
      }
    }.toSeq
  }

  def isTeenth(localDate: LocalDate): Boolean = {
    val dayOfMonth = localDate.getDayOfMonth
    12 < dayOfMonth && dayOfMonth < 20
  }
}

object Schedule extends Enumeration {
  type Schedule = Value
  val Teenth, First, Second, Third, Fourth, Last = Value
}

object Meetup {
  val Mon = DayOfWeek.MONDAY.getValue
  val Tue = DayOfWeek.TUESDAY.getValue
  val Wed = DayOfWeek.WEDNESDAY.getValue
  val Thu = DayOfWeek.THURSDAY.getValue
  val Fri = DayOfWeek.FRIDAY.getValue
  val Sat = DayOfWeek.SATURDAY.getValue
  val Sun = DayOfWeek.SUNDAY.getValue
}
