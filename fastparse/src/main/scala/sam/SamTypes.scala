package sam

/** Represents the SAM file format types and data structures. Based on the
  * SAM/BAM specification v1.6 See http://samtools.github.io/hts-specs/SAMv1.pdf
  */

// SAM file format constants
object SamConstants {
  val FileFormatVersion = "1.6"
  val FileFormatDate = "22 May 2018"
}

// Sorting orders for @HD SO tag
enum SortingOrder:
  case Unknown, Unsorted, Queryname, Coordinate

// Grouping orders for @HD GO tag
enum GroupingOrder:
  case None, Query, Reference

/** Represents a CIGAR operation.
  * @param length
  *   the length of the operation
  * @param op
  *   the operation character (M, I, D, N, S, H, P, =, X)
  */
case class CigarOperation(length: Int, op: Char)

/** Represents the optional tags in SAM alignment records. Format:
  * TAG:TYPE:VALUE
  * @param tag
  *   the two-character tag identifier
  * @param tagType
  *   the single-character type (A, i, f, Z, H, B)
  * @param value
  *   the value as a string
  */
case class OptionalTag(tag: String, tagType: Char, value: String)

/** Represents the header section of a SAM file.
  *
  * Each line (except for @CO) is represented as a Map[String, String], mapping
  * string tags to string values.
  */
case class Header(
    hd: Option[Map[String, String]] = None, // @HD line
    sq: Seq[Map[String, String]] = Seq.empty, // @SQ lines
    rg: Seq[Map[String, String]] = Seq.empty, // @RG lines
    pg: Seq[Map[String, String]] = Seq.empty, // @PG lines
    co: Seq[String] = Seq.empty, // @CO comment lines
    userRecords: Map[String, Seq[Map[String, String]]] =
      Map.empty // User-defined @ tags
)

object Header {
  def empty: Header = Header()

  /** Ensures that an @HD line is present in the header. If an @HD line already
    * exists, it is returned unchanged. Otherwise, a default VN value is added.
    */
  def ensureHD(header: Header): Header =
    if header.hd.isDefined then header
    else header.copy(hd = Some(Map("VN" -> SamConstants.FileFormatVersion)))

  /** Returns the sorting order (SO) stored in the @HD line. */
  def getSortingOrder(header: Header): SortingOrder =
    header.hd
      .flatMap(_.get("SO"))
      .flatMap {
        case "unknown"    => Some(SortingOrder.Unknown)
        case "unsorted"   => Some(SortingOrder.Unsorted)
        case "queryname"  => Some(SortingOrder.Queryname)
        case "coordinate" => Some(SortingOrder.Coordinate)
        case _            => None
      }
      .getOrElse(SortingOrder.Unknown)
}

/** Represents an alignment record in a SAM file. This corresponds to a single
  * line in the alignment section.
  *
  * See http://samtools.github.io/hts-specs/SAMv1.pdf - Section 1.4
  */
case class Alignment(
    qname: String, // Query template NAME
    flag: Int, // Bitwise FLAG
    rname: String, // Reference sequence NAME
    pos: Int, // 1-based leftmost mapping POSition
    mapq: Int, // MAPping Quality
    cigar: Seq[CigarOperation], // CIGAR string as operations
    rnext: String, // Reference name of mate/NEXT read
    pnext: Int, // Position of mate/NEXT read
    tlen: Int, // Observed Template LENgth
    seq: String, // Segment SEQuence
    qual: String, // ASCII of Phred-scaled base QUALity+33
    tags: Seq[OptionalTag] = Seq.empty // Optional fields
)

object Alignment {
  // Flag bit masks
  object Flags {
    val PairedRead = 0x1
    val ProperPair = 0x2
    val Unmapped = 0x4
    val MateUnmapped = 0x8
    val ReverseStrand = 0x10
    val MateReverseStrand = 0x20
    val FirstInPair = 0x40
    val SecondInPair = 0x80
    val SecondaryAlignment = 0x100
    val FailsQualityChecks = 0x200
    val PCROrOpticalDuplicate = 0x400
    val SupplementaryAlignment = 0x800
  }

  def hasFlag(aln: Alignment, flag: Int): Boolean =
    (aln.flag & flag) != 0
}

/** Represents a complete SAM file with header and alignments. */
case class SamFile(
    header: Header,
    alignments: Seq[Alignment]
)
