// https://www.coursera.org/learn/progfun2/lecture/EkEqR/lecture-2-5-case-study-the-water-pouring-problem
// Water poring problem:
// given two glasses: volume 4 units and volume 9 units and a facet
// possible actions:
//   fill any glass from facet,
//   pour water from one glass to another
//   empty any glass
// goal: pour exactly 6 units into a glass

class Pouring (capacity: Vector[Int]) {

  // States
  type State = Vector[Int]
  val initialState = capacity map (x => 0)

  // Moves
  trait Move {
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    override def change(state: State): State =
      state.updated(glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    override def change(state: State): State =
      state.updated(glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    override def change(state: State): State = {
      val amount = state(from).min(capacity(to) - state(to))
      state
        .updated(from, state(from) - amount)
        .updated(to, state(to) + amount)
    }
  }

  val glasses = 0 until capacity.length

  val moves =
    (for (g <- glasses) yield Empty(g)) ++
    (for (g <- glasses) yield Fill(g)) ++
    (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  // Paths

  class Path(history: List[Move], val endState: State) {
//    def endState: State = history.foldRight(initialState)(_.change(_))
//    def endState: State = trackState(history)
//    private def trackState(xs: List[Move]): State = xs match {
//      case Nil => initialState
//      case move :: xs1 => move.change(trackState(xs1))
//    }
    def extend(move: Move) = new Path(move :: history, move.change(endState))
    override def toString = history.reverse.mkString(" ") + "--> " + endState
  }

  val initialPath = new Path(Nil, initialState)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves.map(path.extend)
        if !explored.contains(next.endState)
      } yield next
      paths #:: from(more, explored ++ more.map(_.endState))
    }

  val pathSets = from(Set(initialPath), Set(initialState))

  def solution(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState.contains(target)
    } yield path
}

var problem = new Pouring(Vector(4, 9))
problem.moves

problem.pathSets.take(3).toList

problem.solution(6)

/*
Stream(
Fill(1)
Pour(1,0)
Empty(0)
Pour(1,0)
Empty(0)
Pour(1,0)
Fill(1)
Pour(1,0)
--> Vector(4, 6), ?
)

 */