/*
Написать DSL, который позволяет писать следующий код:

1.usd to rub //актуальная конверсия на день вызова метода по ЦБ
1.rub to eur on 21--10--2014 //конверсия по курсу ЦБ на определенный день

Сделать архитектурно легко расширяемым на другие валюты.
*/

import java.time.LocalDate

import scala.xml.XML

def loadExchangeRate(fromCurrency: String, date: LocalDate): Double = {
  val request = s"http://www.cbr.ru/scripts/XML_daily.asp?date_req=${date.getDayOfMonth}/${date.getMonthValue}/${date.getYear}"
  XML.load(request)
    .child
    .toList
    .filter(_.child.exists(_.text == fromCurrency))
    .head
    .child
    .filter(_.label == "Value")
    .head.text.replace(',', '.').toDouble
}

def getExchangeRate(from: String, to: String, date: LocalDate): Double = {
  if (from == to) return 1
  if (to.equals("RUB")) {
    return loadExchangeRate(from, date)
  }
  if (from.equals("RUB")) {
    return 1d / loadExchangeRate(to, date)
  }
  loadExchangeRate(from, date) / loadExchangeRate(to, date)
}

object Currency extends Enumeration {
  type Currency = Value
  val usd = Value("USD")
  val rub = Value("RUB")
  val eur = Value("EUR")
}

import Currency._

implicit class MoneyFactory(value: Double) {
  val usd = new Money(value, Currency.usd)
  val eur = new Money(value, Currency.eur)
  val rub = new Money(value, Currency.rub)
}

class Money(amount: Double, currency: Currency) {
  var initialValue: Money = _

  def on(date: LocalDate): Money = {
    initialValue.to(currency, date)
  }

  def to(toCurrency: Currency, date: LocalDate = LocalDate.now): Money = {
    val exchangeRate: Double = getExchangeRate(currency.toString, toCurrency.toString, date)
    val converted = new Money(amount * exchangeRate, toCurrency)
    converted.initialValue = this
    converted
  }

  override def toString: String = amount + " " + currency
}

case class Month(day: Int, month: Int) {
  def --(year: Int): LocalDate = LocalDate.of(year, month, day)
}

implicit class Day(day: Int) {
  def --(month: Int): Month = Month(day, month)
}

val rubles: Money = 100.usd to rub
val euro1: Money = 100.usd to eur
val euro2: Money = 1000.rub to eur
val euro3: Money = 1000.rub to eur on 11--11--2011


