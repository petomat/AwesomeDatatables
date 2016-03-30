import scala.util.Try
import org.specs2.Specification

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

// scalastyle:off
// Reason: unit test

// formatter:off

class NeatDatatablesExampleSpecification extends Specification with NeatDatatables {

  def is = s2"""
Neat Datatables
  Example 1
${dataTableExample(example1)}
  Example 2
${dataTableExample(example2)}
  Example 3
${dataTableExample(example3)}
"""

  val evaluate = {
    (decimal: String, fractional: String, fpn: Double) =>
      (for {
        d <- Try(decimal.toInt).toOption
        f <- Try(fractional.toInt).toOption
        if f >= 0D
      } yield {
        d + (d.signum * f / math.pow(10, fractional.length))
      }) must beSome(fpn)
  }



  /**
   * [info] + Decimal Digits | Fractional Digits | Double
   * [info]   0              | 0                 | 0.0
   * [info]   12             | 34                | 12.34
   * [info]   -3             | 141               | -3.141
   */
  def example1 = {
    // Note: Importing the default of how to make a string out of columns
    import ColumnToStringByType.Implicits.Default
    "Decimal Digits" || "Fractional Digits" || "Double" |
      "0" !! "0"                 !!   0D     |
      "12" !! "34"                !!  12.34D  |
      "-3" !! "141"               !!  -3.141D |> {
      evaluate
    }
  }


  def example2 = {
    /**
     * [info] + Decimal Digits    | Fractional Digits | Double
     * [info]                   0 |                 0 |  0,00000
     * [info]                  12 |                34 | 12,34000
     * [info]                  -3 |               141 | -3,14100
     */
    implicit object ColumnToStr extends ColumnToStringByType {
      // PER-TYPE-description of how to make a string out of a column
      // Note: You can leave out any of the following defs, because a default is provided which will use toString
      implicit def caseString = at[String](s => f"$s%17s")
      implicit def caseDouble = at[Double](d => f"$d%8.5f")
    }

    "Decimal Digits" || "Fractional Digits" || "Double" |
      "0" !! "0"                 !!   0D     |
      "12" !! "34"                !!  12.34D  |
      "-3" !! "141"               !!  -3.141D |> {
      evaluate
    }
  }


  def example3 = {
    /**
     * [info] + Decimal Digits | Fractional Digits | Double
     * [info]                0 | 0                 |  0,00000
     * [info]               12 | 34                | 12,34000
     * [info]               -3 | 141               | -3,14100
     */
    implicit object ColumnToStr extends ColumnToStringByIndex {
      import shapeless.tag.@@
      import shapeless.nat._
      // PER-COLUMN-description of how to make a string out of a column
      // Note: You can leave out any of the following defs, because a default is provided which will use toString
      implicit def caseCol1 = at[String @@ _1] { s => f"$s%14s" }
      implicit def caseCol2 = at[String @@ _2] { s => f"$s%-17s" }
      implicit def caseCol3 = at[Double @@ _3] { d => f"$d%8.5f" }
    }

    "Decimal Digits" || "Fractional Digits" || "Double" |
      "0" !! "0"                 !!   0D     |
      "12" !! "34"                !!  12.34D  |
      "-3" !! "141"               !!  -3.141D |> {
      evaluate
    }
  }

}
