import scala.language.implicitConversions
import scala.language.existentials
import scala.annotation.implicitNotFound
import org.specs2.execute._
import org.specs2.matcher.DataTable
import org.specs2.text.TextTable
import shapeless._
import shapeless.nat._
import shapeless.ops.hlist._
import shapeless.ops.function.FnToProduct
import shapeless.ops.sized.ToHList
import shapeless.tag.@@


/**
 * This is an almost drop-in replacement for [[org.specs2.matcher.DataTables]]
 *
 * The main differences to specs2 data tables are:
 * 1)    Pro: Compile time check that table heading has same number of columns as each data row.
 * 2)    Pro: Works for arbitrary number of columns with this fixed source code (no source code generation).
 * 3)    Pro: Per column to-string-conversion either by type or column index for better readability!
 * 3) Contra: Type of evaluation function after |> must be fully specified.
 *
 * See an example at NeatDatatablesExampleSpecification
 */
trait NeatDatatables {

  // scalastyle:off structural.type Reason: typelevel programming à la shapeless inherently uses type refinements for
  //                                        aliasing abstract type parameter via Aux pattern.

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
  trait ColumnToStringByType extends Poly1Fix[String] {
    implicit def caseGeneric[T]: CaseFix[T] = at[T](_.toString)
  }
  object ColumnToStringByType {
    object Implicits {
      implicit object Default extends ColumnToStringByType
    }
  }

  /** Type class witnessing how to convert a column to a string, column number based */
  trait ColumnToStringByIndex extends Poly1Fix[String] {
    implicit def caseNatted[T, N <: Nat](implicit c: CaseFix[T]): CaseFix[T @@ N] = {
      at[T @@ N] { case t => c(t :: HNil) }
    }
    implicit def caseGeneric[T]: CaseFix[T] = at[T](_.toString)
  }

  /** Entry point for constructing a row */
  implicit def toRow[T](t: T): Row[T :: HNil] = Row(t :: HNil)
  case class Row[R <: HList](columns: R) {
    def !![T](t: T)(
      implicit prepend: Prepend[R, T :: HNil]
    ): Row[prepend.Out] = Row(columns :+ t)
  }

  /** Type class witnessing how a row of type R is converted to a Seq[String] */
  @implicitNotFound(
    """
Could not determine how to convert a row (more precisely the columns of the row) to string.
Probably there is no implicit instance of ColumnToStringByType or ColumnToStringByIndex in scope.
Import ColumnToStringByType.Implicits.Default for a simple default.
    """
  )
  trait RowToString[R <: HList] {
    def apply(r: R): Seq[String]
  }
  object RowToString {
    // If this object is private scalac will compile but this gives a NoSuchMethodError at Runtime
    object TagPoly1 extends Poly1 {
      // Succ[N] because we want one-based column indices
      implicit def default[T, N <: Nat]: Case[(T, N)] { type Result = T @@ Succ[N] } = {
        at[(T, N)] { case (t, n) => tag[Succ[N]](t): T @@ Succ[N] }
      }
    }
    /** ColumnToString aka column type based version */
    implicit def byType[R <: HList, CST <: ColumnToStringByType, Out <: HList](
      implicit cst: CST,
      mapper: Mapper.Aux[CST, R, Out],
      toTraversable: ToTraversable.Aux[Out, Vector, String]
    ): RowToString[R] = {
      new RowToString[R] {
        def apply(r: R): Seq[String] = {
          mapper(r).to[Vector]
        }
      }
    }
    /** ColumnToStringIndexed aka column number based version */
    implicit def byIndex[R <: HList, RZ <: HList, RZT <: HList, CSI <: ColumnToStringByIndex, Out <: HList](
      implicit csi: CSI,
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
  @implicitNotFound("Subsequent row of type ${R} has wrong length, should be of length ${N}")
  trait ProperRowLength[R <: HList, N <: Nat]
  object ProperRowLength {
    // scalastyle:off null Reason: Only compile time never-used implicit instance
    implicit def default[R <: HList, N <: Nat](implicit l: Length.Aux[R, N]): ProperRowLength[R, N] = null
    // scalastyle: on null
  }


  /** Entry point for constructing a heading */
  implicit def toHeading(s: String): Heading[_1] = Heading(Sized[Vector](s))
  case class Heading[N <: Nat](columns: Sized[Vector[String], N]) {
    def ||(s: String): Heading[Succ[N]] = Heading(columns :+ s)
    def |>[R <: HList](row: Row[R])(implicit l: ProperRowLength[R, N]): Table[R, _1] = {
      Table(this, Sized[Vector](row.columns))
    }
    def | [R <: HList](row: Row[R])(implicit l: ProperRowLength[R, N]): Table[R, _1] = |>(row)
  }

  /** Type class witnessing the evaluation function passed to |> at the end of a table fits the row type */
  @implicitNotFound("Domain of provided function of type ${F} does not conform to table row type ${R}")
  trait ProperTestFunction[F, R <: HList, Out] {
    def fnTnProd: FnToProduct.Aux[F, R => Out]
  }
  object ProperTestFunction {
    implicit def default[R <: HList, F, Out0](implicit
      fnToProd0: FnToProduct.Aux[F, R => Out0]
    ): ProperTestFunction[F, R, Out0] = new ProperTestFunction[F, R, Out0] {
      type Out = Out0
      val fnTnProd = fnToProd0
    }
  }

  /** Table holding the heading and rows */
  case class Table[R <: HList, N <: Nat](heading: Heading[_ <: Nat], rows: Sized[Vector[R], N]) {
    def titles: Seq[String] = heading.columns.to[Vector]
    def |[R0 <: HList, RHL <: HList, P <: HList, U <: HList, Lub <: HList](row: Row[R0])(implicit
      toHlist: ToHList.Aux[Vector[R], N, RHL],
      prepend: Prepend.Aux[RHL, R0 :: HNil, P],
      toSized: ToSized.Aux[P, Vector, Lub, Succ[N]]
    ): Table[Lub, Succ[N]] = {
      Table(heading, toSized(prepend(toHlist(rows), row.columns :: HNil)))
    }
    def |>[F, Res](f: F)(implicit
      properTestFunction: ProperTestFunction[F, R, Res],
      asResult: AsResult[Res],
      rowToString: RowToString[R]
    ): DecoratedResult[DataTable] = {
      val prodFct: R => Res = properTestFunction.fnTnProd(f)
      Table.collect(titles, rows.to[Vector] map { case cols => rowToString(cols) -> prodFct(cols) })(asResult)
    }
    def |[F, Res](f: F)(implicit
      properTestFunction: ProperTestFunction[F, R, Res],
      asResult: AsResult[Res],
      rowToString: RowToString[R]
    ): DecoratedResult[DataTable] = |>(f)
  }


  // scalastyle:off
  // Reason: license

  /*
   * Copyright (c) 2007-2012 Eric Torreborre <etorreborre@yahoo.com>
   *
   * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
   * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
   * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
   * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
   *
   * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
   * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
   * products derived from this software without specific prior written permission.
   *
   * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
   * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
   * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   * DEALINGS IN THE SOFTWARE.
   *
   * Other licenses:
   *
   * the maven-base.css, maven-theme.css, print.css files come from the Maven project with an Apache license (maven.apache.org/license.html)
   * the tabber css and js files have a MIT license (see he www.barelyfitz.com/projects/tabber site)
   * the collapsible sections on html pages are originally inspired from the ones in Fitnesse (fitnesse.org, GPL license)
   *
   *
   * Credits:
   *
   * to @robey for the eventually matchers
   * to @jedws for the NamedThreadFactory
   * to @alexey_r for the Parser matchers
   */

  // scalastyle:on

  /**
   * THE FOLLOWING CODE ESSENTIALLY HAS BEEN COPIED FROM [org.specs2.matcher.DataTables.Table] because it couldn't
   * be directly used. Method collect was changed in that titles: Seq[String] are passed from the outside. Moreover
   * some access modifier were changed and List was replaced by Seq.
   */

  import ResultLogicalCombinators._
  object Table {
    /** this method can be overridden to throw exceptions when checking the result */
    def checkResultFailure(r: =>Result): Result = r
    /**
     * Collect the results of each row
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
      results.foldLeft(Success("", results.size): Result)((res, cur) => res and AsResult(cur._2))
    }
    /** @return the status of the row + the values + the failure message if any */
    private def resultLine(line: Seq[String], result: Result): Seq[String] = {
      val message = if (result.isSuccess) "" else result.message
      result.status +: line :+ message
    }
  }

}
