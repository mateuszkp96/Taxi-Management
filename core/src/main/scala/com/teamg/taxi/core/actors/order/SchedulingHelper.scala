package com.teamg.taxi.core.actors.order

import java.time.Instant
import java.time.temporal.ChronoUnit

import com.teamg.taxi.core.model.{CustomerType, Order, OrderType}

object SchedulingHelper {
  case class OrderRequest(time: Long, order: Order)

  case class OrderBids(order: Order, bids: Map[String, Double])

  object timesToDecision {
    val normal = 1000
    val vip = 500
    val supervip = 100
  }

  def assignToTaxis(list: List[OrderBids]): Map[String, Order] = {
    val dummy: List[(Order, String)] = list
      .map(orderBids => orderBids.order -> orderBids.bids.minBy(_._2))
      .map(a => (a._1, a._2._1))

//    val dummyAssign = dummy
//      .groupBy(_._2)
//      .filter(entry => entry._2.size == 1)
//      .flatMap(a => a._2)
//      .map(_.swap)

    val dummyAssign = dummy
      .groupBy(_._2)
      .map(_._2.head)
      .map(_.swap)

    dummyAssign
  }

  def getTimeToDecision(order: Order): Instant = {
    order.orderType match {
      case OrderType.Normal =>
        order.customerType match {
          case CustomerType.Normal => order.timeStamp.plus(timesToDecision.normal, ChronoUnit.SECONDS)
          case CustomerType.Vip => order.timeStamp.plus(timesToDecision.vip, ChronoUnit.SECONDS)
          case CustomerType.SuperVip => order.timeStamp.plus(timesToDecision.supervip, ChronoUnit.SECONDS)
        }
      case OrderType.Predefined(time) =>
        order.customerType match {
          case CustomerType.Normal => time.minus(timesToDecision.normal, ChronoUnit.SECONDS)
          case CustomerType.Vip => time.minus(timesToDecision.vip, ChronoUnit.SECONDS)
          case CustomerType.SuperVip => time.minus(timesToDecision.supervip, ChronoUnit.SECONDS)
        }
    }
  }
}
