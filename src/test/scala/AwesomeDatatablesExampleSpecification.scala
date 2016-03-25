import org.specs2.Specification

import scala.util.Try

class AwesomeDatatablesExampleSpecification extends Specification with AwesomeDatatables {

  def is = s2"""
Awesome Datatables
  Example 1
${dataTableExample(awesomeDatatable1)}
  Example 2
${dataTableExample(awesomeDatatable2)}
  Example 3
${dataTableExample(awesomeDatatable3)}
"""


  // Note: Unfortunately we must provide the full type of the eval function after |>


  /**
   * [info] + Decimal Digits | Fractional Digits | Double
   * [info]   0              | 0                 | 0.0
   * [info]   12             | 34                | 12.34
   * [info]   -3             | 141               | -3.141
   */
  def awesomeDatatable1 = {
    // Note: Importing the default of how to make a string out of columns
    import ColumnToString.Implicits.Default
    "Decimal Digits" || "Fractional Digits" || "Double" |
                 "0" !! "0"                 !!   0D     |
                "12" !! "34"                !!  12.34D  |
                "-3" !! "141"               !!  -3.141D |> {
      (decimal: String, fractional: String, fpn: Double) =>
        (for {
          d <- Try(decimal.toInt).toOption
          f <- Try(fractional.toInt).toOption
          if f >= 0D
        } yield {
          d + (d.signum * f / math.pow(10, fractional.length))
        }) must beSome(fpn)
    }
  }


  def awesomeDatatable2 = {
    /**
     * [info] + Decimal Digits    | Fractional Digits | Double
     * [info]                   0 |                 0 |  0,00000
     * [info]                  12 |                34 | 12,34000
     * [info]                  -3 |               141 | -3,14100
     */
    implicit object ColumnToStr extends ColumnToString {
      // PER-TYPE-description of how to make a string out of a column
      // Note: You can leave out any of the following defs, because a default is provided which will use toString
      implicit def caseString = at[String](s => f"$s%17s")
      implicit def caseDouble = at[Double](d => f"$d%8.5f")
    }

    "Decimal Digits" || "Fractional Digits" || "Double" |
                 "0" !! "0"                 !!   0D     |
                "12" !! "34"                !!  12.34D  |
                "-3" !! "141"               !!  -3.141D |> {
      (decimal: String, fractional: String, fpn: Double) =>
        (for {
          d <- Try(decimal.toInt).toOption
          f <- Try(fractional.toInt).toOption
          if f >= 0D
        } yield {
          d + (d.signum * f / math.pow(10, fractional.length))
        }) must beSome(fpn)
    }
  }


  def awesomeDatatable3 = {
    /**
     * [info] + Decimal Digits | Fractional Digits | Double
     * [info]                0 | 0                 |  0,00000
     * [info]               12 | 34                | 12,34000
     * [info]               -3 | 141               | -3,14100
     */
    implicit object ColumnToStr extends ColumnToStringIndexed {
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
      (decimal: String, fractional: String, fpn: Double) =>
        (for {
          d <- Try(decimal.toInt).toOption
          f <- Try(fractional.toInt).toOption
          if f >= 0D
        } yield {
          d + (d.signum * f / math.pow(10, fractional.length))
        }) must beSome(fpn)
    }
  }


}

