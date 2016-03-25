import scala.annotation.implicitNotFound
import org.specs2.execute._
import org.specs2.matcher.DataTable
import org.specs2.text.TextTable
import shapeless._
import shapeless.nat._
import shapeless.ops.hlist._
import shapeless.ops.function.FnToProduct

/**
 * This is an almost drop-in replacement for [[org.specs2.matcher.DataTables]]
 *
 * The main differences to specs2 datatables are:
 * 1) Compile time check that table heading must have same amount of columns as data rows
 * 2) Per column toString conversion either by type or column index for better readability!
 *
 * See an example at AwesomeDatatablesExampleSpecification
 */
trait AwesomeDatatables {

  /** Like [[shapeless.Poly1]] but with fixed return type of functions passed to method "at" */
  trait Poly1Fix[R] extends Poly {
    type Case[A] = poly.Case[this.type, A :: HNil]
    type CaseFix[A] = Case[A] { type Result = R }
    def at[A](f: A => R): CaseFix[A] = new Case[A] {
      type Result = R
      val value = (l: A :: HNil) => l match { case a :: HNil => f(a) }
    }
  }

  /** Type class witnessing how to convert a column to a string, type based */
  trait ColumnToString extends Poly1Fix[String] {
    implicit def caseGeneric[T]: CaseFix[T] = at[T](_.toString)
  }
  object ColumnToString {
    object Implicits {
      implicit object Default extends ColumnToString
    }
  }

  /** Type class witnessing how to convert a column to a string, column number based */
  trait ColumnToStringIndexed extends Poly1Fix[String] {
    import shapeless.tag.@@
    implicit def caseNatted[T, N <: Nat](implicit c: CaseFix[T]): CaseFix[T @@ N] = {
      at[T @@ N] { case t => c(t :: HNil) }
    }
    implicit def caseGeneric[T]: CaseFix[T] = at[T](_.toString)
  }

  /** Entry point to construction a row */
  implicit def toRow[T](t: T): Row[T :: HNil] = Row(t :: HNil)
  case class Row[R <: HList](cols: R) {
    def !![T](t: T)(
      implicit prepend: Prepend[R, T :: HNil]
    ): Row[prepend.Out] = Row(cols :+ t)
  }

  /** Type class witnessing how a row of type R is converted to a Seq[String] */
  trait RowToString[R <: HList] {
    def apply(r: R): Seq[String]
  }
  object RowToString {
    private object TagPoly1 extends Poly1 {
      // Succ[N] because we want one-based column indices
      implicit def default[T, N <: Nat] = at[(T, N)] { case (t, n) => tag[Succ[N]](t) }
    }
    /** ColumnToString aka column type based version */
    implicit def default[R <: HList, CS <: ColumnToString, Out <: HList](
      implicit cs: CS,
      mapper: Mapper.Aux[CS, R, Out],
      toTraversable: ToTraversable.Aux[Out, Vector, String]
    ): RowToString[R] = {
      new RowToString[R] {
        def apply(r: R): Seq[String] = {
          mapper(r).to[Vector]
        }
      }
    }
    /** ColumnToStringIndexed aka column number based version */
    implicit def indexed[R <: HList, RZ <: HList, RZT <: HList, CSI <: ColumnToStringIndexed, Out <: HList](
      implicit cs: CSI,
      zipper: ZipWithIndex.Aux[R, RZ],
      tagMapper: Mapper.Aux[TagPoly1.type, RZ, RZT],
      mapper: Mapper.Aux[CSI, RZT, Out],
      toTraversable: ToTraversable.Aux[Out, Vector, String]
    ): RowToString[R] = {
      new RowToString[R] {
        def apply(r: R): Seq[String] = {
          mapper(tagMapper(zipper(r))).to[Vector]
        }
      }
    }
  }


  /** Type class witnessing a row is proper length N */
  @implicitNotFound("Subsequent row of type ${HL} has wrong length, should be of length ${N}")
  trait ProperRowLength[HL <: HList, N <: Nat]
  object ProperRowLength {
    implicit def default[HL <: HList, N <: Nat](
      implicit l: Length.Aux[HL, N]
    ): ProperRowLength[HL, N] = null
  }


  /** Entry point to construction a heading */
  implicit def toHeading(s: String): Heading[_1] = Heading(Sized[Vector](s))
  case class Heading[N <: Nat](cols: Sized[Vector[String], N]) {
    def ||(s: String): Heading[Succ[N]] = Heading(cols :+ s)
    def |[R <: HList](row: Row[R])(
      implicit l: ProperRowLength[R, N]
    ): Table[R] = Table(this, Vector(row))
  }

  /** Type class witnessing the evaluation function passed to |> at the end of an table fits the row type */
  @implicitNotFound("Provided function of type ${F} does not conform to table row type ${R}")
  trait ProperTestFunction[R <: HList, F] {
    type Out
    def functionToProduct: FnToProduct.Aux[F, R => Out]
    def asResult: AsResult[Out]
  }
  object ProperTestFunction {
    implicit def default[R <: HList, F, Out0](
      implicit fnToProd: FnToProduct.Aux[F, R => Out0],
      asResult0: AsResult[Out0]
    ) = new ProperTestFunction[R, F] {
      type Out = Out0
      val asResult = asResult0
      val functionToProduct = fnToProd
    }
  }

  /** Table holding the heading and rows */
  case class Table[R <: HList](heading: Heading[_ <: Nat], rows: Vector[Row[R]]) {
    def |(row: Row[R]): Table[R] = Table(heading, rows :+ row)
    def |>[F](f: F)(implicit ptf: ProperTestFunction[R, F], rowToString: RowToString[R]): DecoratedResult[DataTable] = {
      val prodFct: R => ptf.Out = ptf.functionToProduct(f)
      Table.collect(titles, rows map { case Row(cols) => rowToString(cols) -> prodFct(cols) })(ptf.asResult)
    }
    def titles: Seq[String] = heading.cols.to[Vector]
    def show(implicit rowToString: RowToString[R]): Seq[Seq[String]] = rows map { row => rowToString(row.cols) }
  }

  /**
   * THE FOLLOWING CODE HAS BEEN COPIED FROM [org.specs2.matcher.DataTables.Table] because it couldn't be directly used.
   * Method collect was changed in that titles: Seq[String] are passed from the outside
   */
  object Table {
    /** this method can be overridden to throw exceptions when checking the result */
    def checkResultFailure(r: =>Result): Result = r
    /**
     * Collect the results of each row
     *
     * @param results list of (row description, row execution result)
     * @return an aggregated Result from a list of results
     */
    def collect[R : AsResult](titles: Seq[String], results: Seq[(Seq[String], R)]): DecoratedResult[DataTable] = {
      val result = allSuccess(results)
      val decorated =
        DecoratedResult(DataTable(titles, results), result.updateMessage {
          TextTable("" +: titles :+ "", results.map { case (line, r) => resultLine(line, AsResult(r)) }:_*).show
        })
      checkResultFailure(decorated)
      decorated
    }
    /** @return the logical and combination of all the results */
    private def allSuccess[R : AsResult](results: Seq[(Seq[String], R)]): Result = {
      import ResultLogicalCombinators._
      results.foldLeft(Success("", results.size): Result)((res, cur) => res and AsResult(cur._2))
    }
    /** @return the status of the row + the values + the failure message if any */
    private def resultLine(line: Seq[String], result: Result): Seq[String] = {
      val message = if (result.isSuccess) "" else result.message
      result.status +: line :+ message
    }
  }

}
