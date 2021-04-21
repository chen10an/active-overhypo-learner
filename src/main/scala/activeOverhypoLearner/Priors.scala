package priors

import learner._

trait PriorSpace {
  // prior hypothesis space of functional forms and causal structures

  // joint
  def hypsDist: Dist[Hyp]
}

case class SimpleSpace(priorPDisj: Double, allBlocks: Set[Block], isPragmatic: Boolean) extends PriorSpace {
  val priorPConj = {
    assert(priorPDisj >= 0.0 && priorPDisj <= 1.0)
    1.0-priorPDisj
  }

  val disj = Fform("disj", (n: Int) => if(n >= 1) 1.0 else 0.0)
  val conj = Fform("conj", (n: Int) => if(n >= 2) 1.0 else 0.0)

  override val hypsDist: Dist[Hyp] = {
    val allFforms = Set(disj, conj)
    val fformDist = Dist(Map(disj -> priorPDisj, conj -> priorPConj))

    // all possible combinations/subsets of blocks
    val allStructs = allBlocks.subsets().toIndexedSeq.map(_.asInstanceOf[Set[Block]])
    val uniformStructDist = Dist[Set[Block]](allStructs.map(blickets => (blickets, 1.0)).toMap)  // unnormalized

    // if isPragmatic, then only consider joint hypotheses where the blickets have a nonzero chance of activating the blicket machine
    val allHyps: IndexedSeq[Hyp] = allFforms.map(fform => allStructs.filter(x => !isPragmatic || (fform.f(x.size) > 0.0)).map(combo => Hyp(combo, fform))).reduce(_ ++ _)
    
    // for each joint hypo, scale the structure probability wrt the form and then normalize
    val unscaledJointDist = Dist[Hyp](allHyps.map(h => (h, uniformStructDist.atoms(h.blickets))).toMap)
    scale(unscaledJointDist).normalize
  }

  private def scale(jointDist: Dist[Hyp]): Dist[Hyp] = {
    // multiply the structure probabilities with a scale factor such that the initial marginal disj:conj probability ratio holds

    // calculate the scale factor x, where x*sum(structures_for_fform) = fformP (assume independence between form and structure)
    val scaleFactors: Map[Fform, Double] = Map(
      disj -> priorPDisj/jointDist.atoms.filter(_._1.fform == disj).map(_._2).sum,
      conj -> priorPConj/jointDist.atoms.filter(_._1.fform == conj).map(_._2).sum
    )

    // multiply with the scale factor
    Dist(jointDist.atoms.map{case (hyp, p) => {hyp -> p*scaleFactors(hyp.fform)}})
  }
}
