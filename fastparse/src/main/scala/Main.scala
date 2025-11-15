import sam._

/** Demo application for the SAM file parser. */
@main def main(args: String*): Unit = {
  if args.isEmpty then
    println("SAM File Parser Demo")
    println("Usage: sbt 'run <path-to-sam-file>'")
    println(
      "\nAlternatively, run without arguments to test with the example file:"
    )
    testWithExampleFile()
  else
    val samFilePath = args(0)
    processSamFile(samFilePath)
}

/** Process and display information about a SAM file. */
def processSamFile(path: String): Unit = {
  println(s"Reading SAM file: $path")
  println("-" * 80)

  SamFiles.readSamFile(path) match {
    case Right(samFile) =>
      displaySamFileInfo(samFile)
    case Left(error) =>
      println(s"Error parsing SAM file: $error")
  }
}

/** Test the parser with the example SAM file in resources. */
def testWithExampleFile(): Unit = {
  val examplePath = "src/main/resources/sam/samExample.sam"
  println(s"\nTesting with example file: $examplePath")
  println("-" * 80)

  SamFiles.readSamFile(examplePath) match {
    case Right(samFile) =>
      displaySamFileInfo(samFile)

      // Display a few example alignments
      println("\nFirst 5 alignments:")
      samFile.alignments.take(5).zipWithIndex.foreach { case (aln, idx) =>
        println(s"\nAlignment ${idx + 1}:")
        println(s"  QNAME: ${aln.qname}")
        println(s"  FLAG: ${aln.flag} (${describeFlagBits(aln.flag)})")
        println(s"  RNAME: ${aln.rname}")
        println(s"  POS: ${aln.pos}")
        println(s"  MAPQ: ${aln.mapq}")
        println(s"  CIGAR: ${formatCigar(aln.cigar)}")
        println(s"  SEQ length: ${aln.seq.length}")
        if aln.tags.nonEmpty then
          println(
            s"  Tags: ${aln.tags.map(t => s"${t.tag}:${t.tagType}:${t.value}").mkString(", ")}"
          )
      }

    case Left(error) =>
      println(s"Error parsing example SAM file: $error")
  }
}

/** Display summary information about a SAM file. */
def displaySamFileInfo(samFile: SamFile): Unit = {
  val header = samFile.header

  println("HEADER INFORMATION:")
  println("-" * 80)

  // Display @HD line
  header.hd match {
    case Some(hd) =>
      println("@HD line:")
      hd.foreach { case (tag, value) =>
        println(s"  $tag: $value")
      }
    case None =>
      println("No @HD line present")
  }
  println()

  // Display @SQ lines
  if header.sq.nonEmpty then
    println(s"@SQ lines (${header.sq.length} reference sequences):")
    header.sq.take(5).foreach { sq =>
      val sn = sq.getOrElse("SN", "?")
      val ln = sq.getOrElse("LN", "?")
      println(s"  $sn (length: $ln)")
    }
    if header.sq.length > 5 then
      println(s"  ... and ${header.sq.length - 5} more")
    println()

  // Display @RG lines
  if header.rg.nonEmpty then
    println(s"@RG lines (${header.rg.length} read groups):")
    header.rg.foreach { rg =>
      val id = rg.getOrElse("ID", "?")
      println(s"  ID: $id")
    }
    println()

  // Display @PG lines
  if header.pg.nonEmpty then
    println(s"@PG lines (${header.pg.length} programs):")
    header.pg.foreach { pg =>
      val id = pg.getOrElse("ID", "?")
      val pn = pg.get("PN")
      println(s"  ID: $id${pn.map(p => s" (PN: $p)").getOrElse("")}")
    }
    println()

  // Display @CO lines
  if header.co.nonEmpty then
    println(s"@CO lines (${header.co.length} comments):")
    header.co.take(3).foreach { comment =>
      println(s"  $comment")
    }
    if header.co.length > 3 then
      println(s"  ... and ${header.co.length - 3} more")
    println()

  // Alignment statistics
  println("ALIGNMENT STATISTICS:")
  println("-" * 80)
  println(s"Total alignments: ${samFile.alignments.length}")

  val mapped =
    samFile.alignments.count(aln => (aln.flag & Alignment.Flags.Unmapped) == 0)
  val unmapped = samFile.alignments.length - mapped
  println(s"Mapped: $mapped")
  println(s"Unmapped: $unmapped")

  val paired = samFile.alignments.count(aln =>
    (aln.flag & Alignment.Flags.PairedRead) != 0
  )
  println(s"Paired reads: $paired")

  val reverseStrand = samFile.alignments.count(aln =>
    (aln.flag & Alignment.Flags.ReverseStrand) != 0
  )
  println(s"Reverse strand: $reverseStrand")

  println()
}

/** Format a CIGAR string for display. */
def formatCigar(cigar: Seq[CigarOperation]): String = {
  if cigar.isEmpty then "*"
  else cigar.map(op => s"${op.length}${op.op}").mkString
}

/** Describe the flag bits that are set. */
def describeFlagBits(flag: Int): String = {
  val flags = scala.collection.mutable.ArrayBuffer[String]()

  if (flag & Alignment.Flags.PairedRead) != 0 then flags += "paired"
  if (flag & Alignment.Flags.ProperPair) != 0 then flags += "proper_pair"
  if (flag & Alignment.Flags.Unmapped) != 0 then flags += "unmapped"
  if (flag & Alignment.Flags.MateUnmapped) != 0 then flags += "mate_unmapped"
  if (flag & Alignment.Flags.ReverseStrand) != 0 then flags += "reverse"
  if (flag & Alignment.Flags.MateReverseStrand) != 0 then
    flags += "mate_reverse"
  if (flag & Alignment.Flags.FirstInPair) != 0 then flags += "first"
  if (flag & Alignment.Flags.SecondInPair) != 0 then flags += "second"
  if (flag & Alignment.Flags.SecondaryAlignment) != 0 then flags += "secondary"
  if (flag & Alignment.Flags.FailsQualityChecks) != 0 then flags += "qc_fail"
  if (flag & Alignment.Flags.PCROrOpticalDuplicate) != 0 then
    flags += "duplicate"
  if (flag & Alignment.Flags.SupplementaryAlignment) != 0 then
    flags += "supplementary"

  if flags.isEmpty then "none" else flags.mkString(", ")
}
