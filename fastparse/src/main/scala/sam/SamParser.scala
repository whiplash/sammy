package sam

import fastparse._
import NoWhitespace._

/** FastParse-based parser for SAM files. Implements parsing according to
  * SAM/BAM specification v1.6 See http://samtools.github.io/hts-specs/SAMv1.pdf
  */
object SamParser {

  // Basic parsers
  def tab(using P[_]): P[Unit] = P("\t")
  def newline(using P[_]): P[Unit] = P("\n" | "\r\n" | "\r")
  def colon(using P[_]): P[Unit] = P(":")

  // Parse until tab or end of line
  def untilTab(using P[_]): P[String] = P(
    CharsWhile(c => c != '\t' && c != '\n' && c != '\r').!
  )
  def untilNewline(using P[_]): P[String] = P(
    CharsWhile(c => c != '\n' && c != '\r').!
  )

  // Parse a field (non-empty string until tab)
  def field(using P[_]): P[String] = P(
    CharsWhile(c => c != '\t' && c != '\n' && c != '\r', min = 1).!
  )

  // Parse an integer
  def integer(using P[_]): P[Int] = P(CharIn("0-9").rep(1).!.map(_.toInt))
  def signedInteger(using P[_]): P[Int] = P(
    ("-".? ~ CharIn("0-9").rep(1)).!.map(_.toInt)
  )

  // Header parsers

  /** Parses a header field (TAG:VALUE format). */
  def headerField(using P[_]): P[(String, String)] = P(
    CharsWhile(c => c != ':' && c != '\t', min = 2).!.filter(
      _.length == 2
    ) ~ ":" ~ untilTab
  )

  /** Parses a header line (excluding @CO lines). Format:
    * \@XX<TAB>TAG:VALUE<TAB>TAG:VALUE...
    */
  def headerLine(using P[_]): P[(String, Map[String, String])] = P(
    "@" ~ CharsWhile(_ != '\t', min = 2).!.filter(_.length == 2) ~ tab ~
      headerField.rep(sep = tab).map(_.toMap)
  )

  /** Parses an @HD line. */
  def hdLine(using P[_]): P[Map[String, String]] = P(
    "@HD" ~ tab ~ headerField.rep(sep = tab).map(_.toMap)
  )

  /** Parses an @SQ line. */
  def sqLine(using P[_]): P[Map[String, String]] = P(
    "@SQ" ~ tab ~ headerField.rep(sep = tab).map(_.toMap)
  )

  /** Parses an @RG line. */
  def rgLine(using P[_]): P[Map[String, String]] = P(
    "@RG" ~ tab ~ headerField.rep(sep = tab).map(_.toMap)
  )

  /** Parses an @PG line. */
  def pgLine(using P[_]): P[Map[String, String]] = P(
    "@PG" ~ tab ~ headerField.rep(sep = tab).map(_.toMap)
  )

  /** Parses an @CO (comment) line. */
  def coLine(using P[_]): P[String] = P(
    "@CO" ~ tab ~ untilNewline
  )

  /** Parses a user-defined header line. */
  def userHeaderLine(using P[_]): P[(String, Map[String, String])] = P(
    "@" ~ CharsWhile(c => c.isLower, min = 2).!.filter(_.length == 2) ~ tab ~
      headerField.rep(sep = tab).map(_.toMap)
  )

  /** Parses the entire header section. */
  def header(using P[_]): P[Header] = P(
    (hdLine.map(Some(_)) | Pass(None)) ~ newline.? ~
      (sqLine ~ newline.?).rep.map(_.toSeq) ~
      (rgLine ~ newline.?).rep.map(_.toSeq) ~
      (pgLine ~ newline.?).rep.map(_.toSeq) ~
      (coLine ~ newline.?).rep.map(_.toSeq)
  ).map { case (hd, sq, rg, pg, co) =>
    Header(hd = hd, sq = sq, rg = rg, pg = pg, co = co)
  }

  // Alignment parsers

  /** Parses a CIGAR string into operations. */
  def cigarOperation(using P[_]): P[CigarOperation] = P(
    integer ~ CharIn("MIDNSHP=X").!
  ).map { case (len, op) => CigarOperation(len, op.head) }

  def cigarString(using P[_]): P[Seq[CigarOperation]] = P(
    P("*").!.map(_ => Seq.empty) | cigarOperation.rep(1).map(_.toSeq)
  )

  /** Parses an optional tag field. Format: TAG:TYPE:VALUE
    */
  def optionalTag(using P[_]): P[OptionalTag] = P(
    CharsWhile(_ != ':', min = 2).!.filter(_.length == 2) ~ ":" ~
      CharIn("AifZHB").! ~ ":" ~
      untilTab
  ).map { case (tag, tpe, value) =>
    OptionalTag(tag, tpe.head, value)
  }

  /** Parses a single alignment record. Format: 11 mandatory fields + optional
    * tags
    */
  def alignment(using P[_]): P[Alignment] = P(
    field ~ tab ~ // QNAME
      integer ~ tab ~ // FLAG
      field ~ tab ~ // RNAME
      integer ~ tab ~ // POS
      integer ~ tab ~ // MAPQ
      cigarString ~ tab ~ // CIGAR
      field ~ tab ~ // RNEXT
      signedInteger ~ tab ~ // PNEXT
      signedInteger ~ tab ~ // TLEN
      field ~ tab ~ // SEQ
      field ~ // QUAL
      (tab ~ optionalTag).rep.map(_.toSeq) // Optional tags
  ).map {
    case (
          qname,
          flag,
          rname,
          pos,
          mapq,
          cigar,
          rnext,
          pnext,
          tlen,
          seq,
          qual,
          tags
        ) =>
      Alignment(
        qname,
        flag,
        rname,
        pos,
        mapq,
        cigar,
        rnext,
        pnext,
        tlen,
        seq,
        qual,
        tags
      )
  }

  /** Parses the alignment section (multiple alignment records). */
  def alignments(using P[_]): P[Seq[Alignment]] = P(
    (alignment ~ newline.?).rep.map(_.toSeq)
  )

  /** Parses a complete SAM file. */
  def samFile(using P[_]): P[SamFile] = P(
    Start ~ header ~ alignments ~ End
  ).map { case (hdr, alns) =>
    SamFile(hdr, alns)
  }

  /** Parse a SAM file from a string.
    * @param input
    *   the SAM file content as a string
    * @return
    *   either a SamFile or a parse error
    */
  def parseSamFile(input: String): Either[String, SamFile] = {
    parse(input, p => samFile(using p)) match {
      case Parsed.Success(result, _) => Right(result)
      case f: Parsed.Failure         => Left(f.trace().longMsg)
    }
  }

  /** Parse only the header section from a string.
    * @param input
    *   the SAM header content
    * @return
    *   either a Header or a parse error
    */
  def parseHeader(input: String): Either[String, Header] = {
    parse(input, p => header(using p)) match {
      case Parsed.Success(result, _) => Right(result)
      case f: Parsed.Failure         => Left(f.trace().longMsg)
    }
  }

  /** Parse a single alignment record from a string.
    * @param input
    *   a single alignment line
    * @return
    *   either an Alignment or a parse error
    */
  def parseAlignment(input: String): Either[String, Alignment] = {
    parse(input, p => alignment(using p)) match {
      case Parsed.Success(result, _) => Right(result)
      case f: Parsed.Failure         => Left(f.trace().longMsg)
    }
  }
}
