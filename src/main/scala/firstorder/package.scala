package object firstorder {

  type Env[A] = List[(String, A)]

  val emptyEnv = Nil

  def lookup[A](env: Env[A])(x: String): A = env match {
    case Nil => sys.error(s"$x not found")
    case (n, v) :: r => if (x == n) v else lookup(r)(x)
  }
}
