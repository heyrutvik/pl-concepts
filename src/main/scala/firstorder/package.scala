package object firstorder {

  type Env[A] = List[(String, A)]

  val emptyEnv = Nil

  def lookup[A](env: Env[A])(x: String): A = env match {
    case Nil => sys.error(s"$x not found")
    case (n, v) :: r => if (x == n) v else lookup(r)(x)
  }

  def union[A](xs: List[A], ys: List[A]): List[A] = xs match {
    case Nil => ys
    case h :: t =>
      if (ys.contains(h)) union(t, ys)
      else h :: union(t, ys)
  }

  def unique[A](xs: List[A]): List[A] = xs.distinct
}
