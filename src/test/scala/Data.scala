import scalaz.{ Scalaz, _ }
import Scalaz._
import java.time.Instant
import org.scalatest.{ BeforeAndAfterAll, FlatSpecLike, Matchers }
import contextual.{ Prefix, Verifier }
object EpochInterpolator extends Verifier[Epoch] {
  override def check(str: String): Either[(Int, String), Epoch] =
    try Right(Epoch(Instant.parse(str).toEpochMilli))
    catch { case _ => Left((0, "not in ISO-8601 format")) }
}

object Data {

  implicit class EpochMillisStringContext(sc: StringContext) {
    val epoch = Prefix(EpochInterpolator, sc)
  }

  val node1   = MachineNode("1243d1af-828f-4ba3-9fc0-a19d86852b5a")
  val node2   = MachineNode("550c4943-229e-47b0-b6be-3d686c5f013f")
  val managed = NonEmptyList(node1, node2)

  val time1: Epoch = epoch"2017-03-03T18:07:00Z"
  val time2: Epoch = epoch"2017-03-03T18:59:00Z" // +52 mins
  val time3: Epoch = epoch"2017-03-03T19:06:00Z" // +59 mins
  val time4: Epoch = epoch"2017-03-03T23:07:00Z" // +5 hours

  val needsAgents = WorldView(5, 0, managed, Map.empty, Map.empty, time1)
}

class Mutable(state: WorldView) extends FlatSpecLike with Matchers {
  import Data._
  var started, stopped: Int = 0
  private val D: Drone[Id] = new Drone[Scalaz.Id] {
    override def getBacklog: Scalaz.Id[Int] = state.backlog
    override def getAgents: Scalaz.Id[Int]  = state.agents
  }

  private val M: Machines[Scalaz.Id] = new Machines[Scalaz.Id] {
    override def getTime: Scalaz.Id[Epoch] = state.time

    override def getManaged: Scalaz.Id[NonEmptyList[MachineNode]] = state.managed

    override def getAlive: Scalaz.Id[Map[MachineNode, Epoch]] = state.alive

    override def start(node: MachineNode): Scalaz.Id[MachineNode] = { started += 1; node }

    override def stop(node: MachineNode): Scalaz.Id[MachineNode] = { stopped += 1; node }
  }

  val program = new DynAgentsModule[Scalaz.Id](D, M)

  "Business Logic" should "generate an initial world view" in {
    val mutable = new Mutable(needsAgents)
    import mutable._
    program.initial shouldBe needsAgents
  }

  it should "remove changed nodes from pending" in {
    val world   = WorldView(0, 0, managed, Map(node1 -> time3), Map.empty, time3)
    val mutable = new Mutable(world)
    import mutable._
    val old = world.copy(alive = Map.empty, pending = Map(node1 -> time2), time = time2)
    program.update(old) shouldBe world
  }

  it should "request agents when needed" in {
    val mutable = new Mutable(needsAgents)
    import mutable._
    val expected = needsAgents.copy(
      pending = Map(node1 -> time1)
    )

    program.act(needsAgents) shouldBe expected
    mutable.stopped shouldBe 0
    mutable.started shouldBe 1
  }

}
