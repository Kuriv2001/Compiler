package alpine
package driver

import alpine.util.toSubstring

import scala.Console
import java.io.OutputStream

/** The configuration of process driving a compilation pipeline.
 *
 *  @param inputs The sources to parse.
 *  @param traceInference `true` if the typer should log a trace of type inference.
 *  @param standardOutput The standard output of the pipeline.
 *  @param standardError The standard error of the pipeline.
 *  @param nodeDebug `true` if the WebAssembly binary should be run with NodeJS in debug mode.
 */
final case class Configuration(
    val inputs: IArray[SourceFile],
    val traceInference: Boolean = false,
    val standardOutput: OutputStream = Console.out,
    val standardError: OutputStream = Console.err,
    
    val nodeDebug: Boolean = false
)
