package com.github.fayezosaadi.fpinscala
package ch1

object FP extends App {

  // pure functions with no side effects

  // referential transparency (RT) and substitution model application on pure functions.

  // RT enables equational reasoning about programs.

  // immutable

  // idempotent

  // modular

  // composable

  // example
  // implement a program to handle purchases at a coffee shop


  class Coffee {
    val price: Double = 3.5
  }

  class CreditCard

  case class Charge(cc: CreditCard, amount: Double) {
    // combine charges with the same credit card
    def combine(other: Charge): Charge = {
      if cc == other.cc then Charge(cc, amount + other.amount) else throw new Exception("Can't combine charges to different cards")
    }
  }

  class Cafe {

    def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
      val cup = new Coffee()
      (cup, Charge(cc, cup.price))
    }

    def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
      val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
      val (coffees, charges): (List[Coffee], List[Charge]) = purchases.unzip
      (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
    }

    // coalesce any same-card charges
    def coalesce(charges: List[Charge]): List[Charge] = {
      charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
    }
  }
}