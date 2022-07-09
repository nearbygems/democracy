package democracy

enum Grade:
  case Bad, Mediocre, Inadequate, Passable, Good, VeryGood, Excellent

object Grade:
  def median(grades: Seq[Grade]): Grade = grades.sortBy(grade => grade.ordinal)
                                                .apply(grades.size / 2)

case class Candidate(name: String)

case class Ballot(grades: Map[Candidate, Grade])

case class Election(description: String, candidates: Set[Candidate]):

  def elect(ballots: Seq[Ballot]): Candidate =
    assert(ballots.nonEmpty)
    assert(ballots.forall(_.grades.keySet == candidates))

    val allGrades: Seq[(Candidate, Grade)] = ballots.flatMap(ballot => ballot.grades)

    val gradesPerCandidate: Map[Candidate, Seq[Grade]] = allGrades.groupMap(p => p._1)(p => p._2)

    findWinner(gradesPerCandidate)

  def findWinner(gradesPerCandidate: Map[Candidate, Seq[Grade]]): Candidate =

    if gradesPerCandidate.forall((candidate, grades) => grades.isEmpty) then

      val candidatesSeq = gradesPerCandidate.keys.toSeq
      val randomIndex   = util.Random.between(0, candidatesSeq.size)
      candidatesSeq(randomIndex)

    else

      val bestMedianGrade: Grade = gradesPerCandidate.values
                                                     .filter(grades => grades.nonEmpty)
                                                     .map(grades => Grade.median(grades))
                                                     .maxBy(grade => grade.ordinal)

      val bestCandidates: Map[Candidate, Seq[Grade]] = gradesPerCandidate
        .filter(p => Grade.median(p._2) == bestMedianGrade)

      if bestCandidates.size == 1 then bestCandidates.head._1
      else

        val bestCandidatesMinusOneMedianGrade: Map[Candidate, Seq[Grade]] =
          bestCandidates.map(p => (p._1, p._2.diff(Seq(Grade.median(p._2)))))

        findWinner(bestCandidatesMinusOneMedianGrade)


