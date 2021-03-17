package example

object BlicketEntropy {

  val conj = Fform("conj",(n: Int) => if(n > 1) 1.0 else 0.0)
  val disj = Fform("disj",(n: Int) => if(n > 0) 1.0 else 0.0)

  def demo(priorPBlicket: Double, priorPConj: Double) = {

    case class Blk(name: String) extends Block {override def toString=name}
    val allBlocks = Set("A","B"/*,"C","D","E","F","G","H","I"*/).map(Blk(_)) // Could also use case objects

    val allCombinations:IndexedSeq[Set[Block]] = allBlocks.subsets().toIndexedSeq.map(_.asInstanceOf[Set[Block]]) // All possible combinations of blocks
    
    val allEvents = allCombinations.map(s => Event(s,true)) ++ allCombinations.map(s => Event(s,false))
    val allHyps = allCombinations.map(s => Hyp(s,conj)) ++ allCombinations.map(s => Hyp(s,disj))

    println(s"Possible blocks: $allBlocks; total possible events: ${allEvents.size}")

    val ffPrior = Map(conj -> priorPConj, disj -> (1.0-priorPConj))
    // def nbPrior(nBlickets: Int, pBlicket: Double, totalObjs: Int): Double = {math.pow(pBlicket,nBlickets)*math.pow(1.0-pBlicket,totalObjs-nBlickets)}

    def uniformPrior(nBlickets: Int, pBlicket: Double, totalObjs: Int): Double = {pBlicket}
    val pHyps = Dist[Hyp](allHyps.map(h => (h,uniformPrior(h.blickets.size,priorPBlicket,allBlocks.size)*ffPrior(h.fform))).toMap).normalize
    println(s"Marginal probability of conjunctive relationship: ${pConjunctive(pHyps)}")
    maxInfo(pHyps,allCombinations)
    println("--------")
    println("Now suppose we've seen A -> false")
    val pHyps2 = conditionalHyps(Event(Set(Blk("A")),false),pHyps)
    println(s"Marginal probability of conjunctive relationship: ${pConjunctive(pHyps2)}")
    maxInfo(pHyps2,allCombinations)
  }

  def maxInfo(pHyps: Dist[Hyp],allCombinations: IndexedSeq[Set[Block]]) = {
    println(s"Entropy of prior: ${pHyps.entropy}")
    val condE = allCombinations.map(v => (v,expectedEntropy(v,pHyps)))
    println("Expected entropy in nats, conditional on interventions:")
    println(condE.sortBy(_._2).mkString("\n"))
  }

  def pConjunctive(pHyps: Dist[Hyp]): Double = {
    pHyps.atoms.filter(_._1.fform == conj).map(_._2).sum
  }

  def expectedEntropy(bs: Set[Block], hypPrior: Dist[Hyp]) = {
    val po = pOutcome(bs,hypPrior)
    Vector((true,po),(false,1.0-po)).map(op => if(op._2 == 0.0) 0.0 else op._2*conditionalHyps(Event(bs,op._1),hypPrior).entropy).sum
  }

  def conditionalHyps(e: Event,hypPrior: Dist[Hyp]): Dist[Hyp] = {
    Dist(hypPrior.atoms.map(p => {
    p._1 -> p._2*likelihood(e,p._1)})).normalize}


  def pOutcome(blocks: Set[Block],hypD: Dist[Hyp]): Double = {
    // [1/0 whether machine activates for a hypotheses]*[probability of that hypothesis]
    // where each hypothesis is a pair of [structure/blickets, form]
    hypD.atoms.map(hp => pActivate(blocks,hp._1)*hp._2).sum
  }

  def pActivate(blocks: Set[Block], hyp: Hyp): Double = {
    hyp.fform.f(blocks.count(b => hyp.blickets.contains(b)))
  }

  def likelihood(e: Event, hyp: Hyp) = {
    val pt = pActivate(e.blocks,hyp)
    if(e.outcome) pt else 1.0-pt
  }
}

case class Fform(name: String, f: Int => Double) {
  override def toString = name
}

trait Block

case class Event(blocks: Set[Block], outcome: Boolean)
case class Hyp(blickets: Set[Block], fform: Fform) 

case class Dist[T](atoms: Map[T,Double]) {
  lazy val entropy = {
    -atoms.values.map(v => if(v == 0) 0.0 else v*math.log(v)).sum
  }

  def normalize = {
    val z = atoms.values.sum
    Dist(atoms.map(p => p._1 -> p._2/z))
  }

  def pp = {
    atoms.mkString("\n")
  }
}

object Main extends App {
  BlicketEntropy.demo(priorPBlicket=.5,priorPConj=.1)
  BlicketEntropy.demo(priorPBlicket=.5,priorPConj=.5)
}
