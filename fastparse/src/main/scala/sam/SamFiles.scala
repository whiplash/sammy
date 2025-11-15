package sam

import scala.io.Source
import scala.util.{Try, Using}
import java.io.{File, PrintWriter}

/** Utilities for reading and writing SAM files. */
object SamFiles {

  /** Read a SAM file from disk.
    *
    * @param path
    *   the path to the SAM file
    * @return
    *   either a SamFile or an error message
    */
  def readSamFile(path: String): Either[String, SamFile] = {
    Try {
      Using(Source.fromFile(path)) { source =>
        val content = source.mkString
        content
      }.get
    }.toEither.left
      .map(e => s"Error reading file: ${e.getMessage}")
      .flatMap(content => SamParser.parseSamFile(content))
  }

  /** Read a SAM file from a File object.
    *
    * @param file
    *   the SAM file
    * @return
    *   either a SamFile or an error message
    */
  def readSamFile(file: File): Either[String, SamFile] = {
    readSamFile(file.getPath)
  }

  /** Read only the header from a SAM file. This is more efficient than reading
    * the entire file if you only need the header.
    *
    * @param path
    *   the path to the SAM file
    * @return
    *   either a Header or an error message
    */
  def readHeader(path: String): Either[String, Header] = {
    Try {
      Using(Source.fromFile(path)) { source =>
        // Read only header lines (those starting with @)
        val headerLines = source
          .getLines()
          .takeWhile(_.startsWith("@"))
          .mkString("\n")
        headerLines
      }.get
    }.toEither.left
      .map(e => s"Error reading file: ${e.getMessage}")
      .flatMap(headerContent => SamParser.parseHeader(headerContent))
  }

  /** Write a SAM file to disk.
    *
    * @param samFile
    *   the SAM file data to write
    * @param path
    *   the output path
    * @return
    *   either Unit on success or an error message
    */
  def writeSamFile(samFile: SamFile, path: String): Either[String, Unit] = {
    Try {
      Using(new PrintWriter(path)) { writer =>
        // Write header
        writeHeader(samFile.header, writer)

        // Write alignments
        samFile.alignments.foreach { aln =>
          writeAlignment(aln, writer)
        }
      }.get
    }.toEither.left.map(e => s"Error writing file: ${e.getMessage}")
  }

  /** Write a SAM file to a File object.
    *
    * @param samFile
    *   the SAM file data to write
    * @param file
    *   the output file
    * @return
    *   either Unit on success or an error message
    */
  def writeSamFile(samFile: SamFile, file: File): Either[String, Unit] = {
    writeSamFile(samFile, file.getPath)
  }

  // Private helper methods

  private def writeHeader(header: Header, writer: PrintWriter): Unit = {
    // Write @HD line if present
    header.hd.foreach { hd =>
      writer.print("@HD")
      hd.foreach { case (tag, value) =>
        writer.print(s"\t$tag:$value")
      }
      writer.println()
    }

    // Write @SQ lines
    header.sq.foreach { sq =>
      writer.print("@SQ")
      sq.foreach { case (tag, value) =>
        writer.print(s"\t$tag:$value")
      }
      writer.println()
    }

    // Write @RG lines
    header.rg.foreach { rg =>
      writer.print("@RG")
      rg.foreach { case (tag, value) =>
        writer.print(s"\t$tag:$value")
      }
      writer.println()
    }

    // Write @PG lines
    header.pg.foreach { pg =>
      writer.print("@PG")
      pg.foreach { case (tag, value) =>
        writer.print(s"\t$tag:$value")
      }
      writer.println()
    }

    // Write @CO lines
    header.co.foreach { comment =>
      writer.println(s"@CO\t$comment")
    }

    // Write user-defined header lines
    header.userRecords.foreach { case (tag, records) =>
      records.foreach { record =>
        writer.print(s"@$tag")
        record.foreach { case (fieldTag, value) =>
          writer.print(s"\t$fieldTag:$value")
        }
        writer.println()
      }
    }
  }

  private def writeAlignment(aln: Alignment, writer: PrintWriter): Unit = {
    import aln._

    // Write mandatory fields
    writer.print(qname)
    writer.print(s"\t$flag")
    writer.print(s"\t$rname")
    writer.print(s"\t$pos")
    writer.print(s"\t$mapq")

    // Write CIGAR
    writer.print("\t")
    if cigar.isEmpty then writer.print("*")
    else
      cigar.foreach { op =>
        writer.print(s"${op.length}${op.op}")
      }

    writer.print(s"\t$rnext")
    writer.print(s"\t$pnext")
    writer.print(s"\t$tlen")
    writer.print(s"\t$seq")
    writer.print(s"\t$qual")

    // Write optional tags
    tags.foreach { tag =>
      writer.print(s"\t${tag.tag}:${tag.tagType}:${tag.value}")
    }

    writer.println()
  }

  /** Format a header as a SAM string. */
  def formatHeader(header: Header): String = {
    val sb = new StringBuilder()

    header.hd.foreach { hd =>
      sb.append("@HD")
      hd.foreach { case (tag, value) =>
        sb.append(s"\t$tag:$value")
      }
      sb.append("\n")
    }

    header.sq.foreach { sq =>
      sb.append("@SQ")
      sq.foreach { case (tag, value) =>
        sb.append(s"\t$tag:$value")
      }
      sb.append("\n")
    }

    header.rg.foreach { rg =>
      sb.append("@RG")
      rg.foreach { case (tag, value) =>
        sb.append(s"\t$tag:$value")
      }
      sb.append("\n")
    }

    header.pg.foreach { pg =>
      sb.append("@PG")
      pg.foreach { case (tag, value) =>
        sb.append(s"\t$tag:$value")
      }
      sb.append("\n")
    }

    header.co.foreach { comment =>
      sb.append(s"@CO\t$comment\n")
    }

    sb.toString()
  }

  /** Format an alignment as a SAM string. */
  def formatAlignment(aln: Alignment): String = {
    import aln._

    val cigarStr =
      if cigar.isEmpty then "*"
      else cigar.map(op => s"${op.length}${op.op}").mkString

    val tagsStr =
      if tags.isEmpty then ""
      else
        "\t" + tags.map(t => s"${t.tag}:${t.tagType}:${t.value}").mkString("\t")

    s"$qname\t$flag\t$rname\t$pos\t$mapq\t$cigarStr\t$rnext\t$pnext\t$tlen\t$seq\t$qual$tagsStr"
  }
}
