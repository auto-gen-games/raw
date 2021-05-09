package rawmaterials

package object game {
  sealed trait AllocateView
  case object DepositsView extends AllocateView
  case object ProducersView extends AllocateView
  case object BalanceView extends AllocateView
  case object BuildView extends AllocateView
  case object TransportView extends AllocateView
  case object SiegeView extends AllocateView
}
