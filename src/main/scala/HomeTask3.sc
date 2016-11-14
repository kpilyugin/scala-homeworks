/*
Написать DSL, который позволяет писать следующий код:

1.usd to rub //актуальная конверсия на день вызова метода по ЦБ
1.rub to eur on 21--10--2014 //конверсия по курсу ЦБ на определенный день

Сделать архитектурно легко расширяемым на другие валюты.
*/

object Currency extends Enumeration {
  type Currency = Value
  val usd, eur, rub = Value
}

import java.time.LocalDate

import Currency._

type ExchangeRate = Function[LocalDate, Double]
type Conversion = Map[(Currency, Currency), ExchangeRate]

def usbToRub(date: LocalDate): Double = if (date.getYear < 2015) 30 else 65
def eurToRub(date: LocalDate): Double = if (date.getYear < 2015) 40 else 71

val conversion: Conversion = Map(
  (usd, rub) -> usbToRub,
  (eur, rub) -> eurToRub
)

case class Converter(conversion: Conversion) extends {
  def convert(from: Currency, to: Currency, date: LocalDate): Double = {
    if (from == to) return 1
    if (conversion.contains((from, to))) {
      conversion((from, to)).apply(date)
    } else {
      1d / conversion((to, from)).apply(date)
    }
  }
}
implicit val converter = Converter(conversion)

val now = LocalDate.now

class Money(amount: Double, currency: Currency) {
  var initialValue: Money = _

  def on(date: LocalDate): Money = {
    initialValue.to(currency, date)
  }

  def to(toCurrency: Currency, date: LocalDate = LocalDate.now): Money = {
    val converted = new Money(amount * converter.convert(currency, toCurrency, date), toCurrency)
    converted.initialValue = this
    converted
  }

  override def toString: String = amount + " " + currency
}

implicit class DoubleMoney(value: Double) {
  val usd = new Money(value, Currency.usd)
  val rub = new Money(value, Currency.rub)
  val eur = new Money(value, Currency.eur)
}

case class Month(day: Int, month: Int) {
  def --(year: Int): LocalDate = LocalDate.of(year, month, day)
}

implicit class Day(day: Int) {
  def --(month: Int): Month = Month(day, month)
}

val rubles = 100.usd to rub
val euro = 100.rub to eur on 12--11--2013


