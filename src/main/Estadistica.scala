package main
import Numeric.Implicits._
object Estadistica extends App {
  def capturarNumeros(cantidad: Int): Vector[Int] = {
    Vector.fill(cantidad)(scala.io.StdIn.readInt())
  }

  def mostrarNumeros(lista:Vector[Int]): Unit = {
    println(lista)
  }

  def mediana[T: Ordering, F]
  (seq: Seq[T])
  (implicit conv: T => F, f: Fractional[F]): F = {
    val secuenciaOrdenada = seq.sorted
    if (seq.size % 2 == 1) secuenciaOrdenada(secuenciaOrdenada.size / 2)  else {
      val (arriba, abajo) = secuenciaOrdenada.splitAt(seq.size / 2)
      import f._
      (conv(arriba.last) + conv(abajo.head)) / fromInt(2)
    }
  }


  def moda[T: Numeric](xs: Iterable[T]):Int = xs.groupBy(i => i).mapValues(_.size).maxBy(_._2)._1.toInt

  def media[T: Numeric](xs: Iterable[T]): Double = xs.sum.toDouble / xs.size

  def varianza[T: Numeric](xs: Iterable[T]): Double = {
    val avg = media(xs)
    xs.map(_.toDouble).map(a => math.pow(a - avg, 2)).sum / xs.size
  }

  def desviacionEstandar[T: Numeric](xs: Iterable[T]): Double = math.sqrt(varianza(xs))

  // PRUEBA
  println("Cuantos Numeros va a ingresar?")
  val n = scala.io.StdIn.readInt()
  println(s"Ingrese $n numeros...")
  val listaNumeros = capturarNumeros(n)
  mostrarNumeros(listaNumeros)
  println("Mediana:"+mediana[Int,Double](listaNumeros))
  println("Moda:"+moda(listaNumeros))
  println("Media:"+media(listaNumeros))
  println("Varianza:"+varianza(listaNumeros))
  println("Desviación estándar (σ):"+desviacionEstandar(listaNumeros))




}
