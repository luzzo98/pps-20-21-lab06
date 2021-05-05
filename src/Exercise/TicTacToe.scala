package Exercise

object TicTacToe extends App{
  sealed trait Player{
    def other: Player = this match {case X => O; case _ => X}
    override def toString: String = this match {case X => "X"; case _ => "O"}
  }
  case object X extends Player
  case object O extends Player

  case class Mark(x: Int, y: Int, player: Player)
  type Board = List[Mark]
  type Game = List[Board]

  def find(board: Board, x: Int, y: Int): Option[Player] = {
    // second in-line version
    board.find(v => v.x==x && v.y==y).map(_.player) // find already give Option.empty if there isn't the element
  }

  def winOnRow(board: Board): Option[Player] = {
    var res = Option.empty :Option[Player]
    for (x <- 0 to 2) {
      if (find(board, x, 0).isDefined && find(board, x, 1).isDefined && find(board, x, 2).isDefined) {
        if (find(board, x, 0).get == find(board, x, 1).get && find(board, x, 1).get == find(board, x, 2).get)
          res = find(board, x, 0)
      }
    }
    res
  }

  def winOnColumn(board: Board): Option[Player] = {
    var res = Option.empty :Option[Player]
    for (x <- 0 to 2) {
      if (find(board, 0, x).isDefined && find(board, 1, x).isDefined && find(board, 2, x).isDefined) {
        if (find(board, 0, x).get == find(board, 1, x).get && find(board, 1, x).get == find(board, 2, x).get)
          res = find(board, 0, x)
      }
    }
    res
  }

  def winOnDiagonal(board: Board): Option[Player] = {
    var res = Option.empty :Option[Player]
    if (find(board, 1, 1).isDefined) {
      if (find(board, 0, 0).isDefined && find(board, 2, 2).isDefined)
        if (find(board, 0, 0).get == find(board, 1, 1).get && find(board, 1, 1).get == find(board, 2, 2).get)
          res = find(board, 1, 1)

      if (find(board, 2, 0).isDefined && find(board, 0, 2).isDefined)
        if (find(board, 2, 0).get == find(board, 1, 1).get && find(board, 1, 1).get == find(board, 0, 2).get)
          res = find(board, 1, 1)
    }
    res
  }

  def placeAnyMark(board: Board, player: Player): Seq[Board] = {
    // third implementation
    for (x <- 0 to 2; y <- 0 to 2; if find(board,x,y).isEmpty) yield Mark(x,y,player) :: board
  }

  def placeAnyMark2(board: Board, player: Player): Seq[Board] = {
    for {
      x <- 0 to 2
      y <- 0 to 2
      if !board.exists(v => v.x == x && v.y == y) && thereIsAWinner(board).isEmpty
    } yield board :+ Mark(x,y,player)
  }

  def thereIsAWinner(board: Board): Option[Player] = {
    var res = winOnRow(board)
    res = if (winOnColumn(board).isEmpty) res else winOnColumn(board)
    res = if (winOnDiagonal(board).isEmpty) res else winOnDiagonal(board)
    res
  }

  def computeAnyGame(player: Player, moves: Int): Stream[Game] = moves match {
    case 0 => Stream(List(Nil))
    case _ => for {
      games <- computeAnyGame(player.other, moves-1)
      game <- placeAnyMark2(games.head,player)
    } yield game :: games
  }

  def printBoards(game: Seq[Board]): Unit = {
    for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
      print(find(board, x, y) map (_.toString) getOrElse ".")
      if (x == 2) { print(" "); if (board == game.head) println()}
    }
  }

//  // Exercise 1: implement find such that..
//  println(find(List(Mark(0,0,X)),0,0)) // Some(X)
//  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),0,1)) // Some(O)
//  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),1,1)) // None

//  // Exercise 2: implement placeAnyMark such that..
//  printBoards(placeAnyMark(List(),X))
//  //... ... ..X ... ... .X. ... ... X..
//  //... ..X ... ... .X. ... ... X.. ...
//  //..X ... ... .X. ... ... X.. ... ...
//  println
//  printBoards(placeAnyMark(List(Mark(0,0,O)),X))
//  //O.. O.. O.X O.. O.. OX. O.. O..
//  //... ..X ... ... .X. ... ... X..
//  //..X ... ... .X. ... ... X.. ...

  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..

//  printBoards(placeAnyMark2(List(Mark(0,0,X),Mark(0,1,X),Mark(0,2,X)),X))

//  var i = 0
//  computeAnyGame(O, 5) foreach {
//    g => printBoards(g)
//    println()
//    i+=1
//    if (g.size<6)
//        println("--------------------------------------------------------")
//  }
//  println(i)

  //... X.. X.. X.. XO.
  //... ... O.. O.. O..
  //... ... ... X.. X..
  //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
  //
  //... ... .O. XO. XOO
  //... ... ... ... ...
  //... .X. .X. .X. .X.

  // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
}


//  def find(board: Board, x: Int, y: Int): Option[Player] = {
//    // first version
//    var res: Option[Player] = Option.empty
//    for (mark <- board) {
//      mark match {
//        case Mark(x2, y2, player) => if (x2==x && y2==y) res = Option(player)
//      }
//    }
//    res
//  }

//  def placeAnyMark(board: Board, player: Player): Seq[Board] = {
//    // second implementation
//    for {
//      x <- 0 to 2
//      y <- 0 to 2
//      if !board.exists(v => v.x == x && v.y == y)
//    } yield board :+ Mark(x,y,player)
//
//    // first implementation
//    var res: Seq[Board] = collection.immutable.Seq.empty
//    for (x <- 0 to 2) {
//      for (y <- 0 to 2) {
//        if (!board.exists(v => v.x == x && v.y == y))
//          res = res :+ (board :+ Mark(x,y,player))
//      }
//    }
//    res
//  }

//  def thereIsAWinner(board: Board): Option[Player] = {
//    var res: Option[Player] = Option.empty
//    for (x <- 0 to 2) {
//      if (find(board, x, 0).isDefined && find(board, x, 1).isDefined && find(board, x, 2).isDefined) {
//        if (find(board, x, 0).get == find(board, x, 1).get && find(board, x, 1).get == find(board, x, 2).get)
//          res = Option(find(board, x, 0).get)
//      }
//      if (find(board, 0, x).isDefined && find(board, 1, x).isDefined && find(board, 2, x).isDefined) {
//        if (find(board, 0, x).get == find(board, 1, x).get && find(board, 1, x).get == find(board, 2, x).get)
//          res = Option(find(board, 0, x).get)
//      }
//    }
//    if (find(board, 0, 0).isDefined && find(board, 1, 1).isDefined && find(board, 2, 2).isDefined) {
//      if (find(board, 0, 0).get == find(board, 1, 1).get && find(board, 1, 1).get == find(board, 2, 2).get)
//        res = Option(find(board, 1, 1).get)
//    }
//    if (find(board, 2, 0).isDefined && find(board, 1, 1).isDefined && find(board, 0, 2).isDefined) {
//      if (find(board, 2, 0).get == find(board, 1, 1).get && find(board, 1, 1).get == find(board, 0, 2).get)
//        res = Option(find(board, 1, 1).get)
//    }
//    res
//  }
