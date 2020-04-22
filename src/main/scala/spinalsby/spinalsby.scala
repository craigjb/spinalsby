package spinalsby

import spinal.core._
import sys.process._
import java.io.File
import java.io.ByteArrayInputStream
import java.nio.file.Paths
import java.nio.file.Files

class SpinalSbyException extends Exception {}

object SbyMode extends Enumeration {
    type SbyMode = Value

    val Bmc   = Value("bmc")
    val Prove = Value("prove")
    val Live  = Value("live")
    val Cover = Value("cover")
    val Equiv = Value ("equiv")
    val Synth = Value("synth")
}

sealed trait SbyEngine {
    def command : String
}

object SmtBmcSolver extends Enumeration {
    type SmtBmcSolver = Value

    val Yices     = Value("yices")
    val Boolector = Value("booleactor")
    val Z3        = Value("z3")
    val mathsat   = Value("mathsat")
    val cvc4      = Value("cvc4")
} 

case class SmtBmc(
    nomem    : Boolean = false,
    syn      : Boolean = false,
    stbv     : Boolean = false,
    stdt     : Boolean = false,
    nopresat : Boolean = false,
    unroll   : Option[Boolean] = None,
    dumpsmt2 : Boolean = false,
    progress : Boolean = true,
    solver   : SmtBmcSolver.SmtBmcSolver = SmtBmcSolver.Yices
) extends SbyEngine {
    def command : String = {
        "smtbmc" +
        (if (nomem) { " --nomem" } else { "" }) +
        (if (syn) { " --syn" } else { "" }) +
        (if (stbv) { " --stbv" } else { "" }) +
        (if (stdt) { " --stdt" } else { "" }) +
        (if (nopresat) { " --nopresat" } else { "" }) +
        (unroll.map(on => {
            if (on) { " --unroll" } else { "--nounroll" }
        }).getOrElse("")) +
        (if (dumpsmt2) { " --dumpsmt2" } else { "" }) +
        (if (progress) { " --progress" } else { "" }) +
        s" $solver"
    }
}

case class Aiger() extends SbyEngine {
    def command : String = { "aiger" }
}

case class Abc() extends SbyEngine {
    def command : String = { "abc" }
}

case class SpinalSbyConfig(
    mode       : SbyMode.SbyMode = SbyMode.Bmc,
    depth      : Int = 100,
    multiClock : Boolean = false,
    timeout    : Option[Int] = None,
    engines    : Seq[SbyEngine] = Seq(SmtBmc()),
    directory  : String = "formal",
    sbyCommand : String = "sby"
) {
    def sbyScript(sources: Seq[String], top: String) : String = {
        val formalPath = Paths.get(directory)
        val localSources = sources.map(f => {
            Paths.get(f).getFileName
        })
        val read = localSources.mkString(" ")
        val engineCmds = engines.map(engine => engine.command).mkString("\n")
        "[options]\n" +
        s"mode $mode\n" +
        s"depth $depth\n" +
        (timeout.map(t => s"timeout $t\n").getOrElse("")) +
        (if (multiClock) { "multiclock on\n" } else { "" }) +
        "\n" +
        "[engines]\n" +
        engineCmds +
        "\n\n" +
        "[script]\n" +
        s"read -formal $read\n" +
        s"prep -top $top\n" +
        "\n" +
        "[files]\n" +
        localSources.mkString("\n")
    }

    def run[T <: Component](config: SpinalConfig, top: => T) {
        val formalDir = new File(directory)
        if (!formalDir.exists()) {
            formalDir.mkdir()
        } 
        val spinalConfig = config.copy(targetDirectory = directory)
        val spinalReport = spinalConfig.includeFormal.generateSystemVerilog(top)
        val topName = spinalReport.toplevel.definitionName
        val script = sbyScript(
            spinalReport.generatedSourcesPaths.toSeq,
            topName
        )
        val scriptName = topName + ".sby"
        Files.write(
            Paths.get(directory, scriptName),
            script.getBytes()
        )
        val exitCode = Process(
            Seq(sbyCommand, "-f", scriptName),
            new java.io.File(directory)).!
        if (exitCode != 0) {
            throw new SpinalSbyException
        }
    }
}
