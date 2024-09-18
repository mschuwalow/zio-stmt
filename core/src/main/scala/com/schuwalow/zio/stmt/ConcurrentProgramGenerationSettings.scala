package com.schuwalow.zio.stmt

final case class ConcurrentProgramGenerationSettings(
  minProgramSize: Int,
  maxProgramSize: Int,
  minConcurrentSteps: Int,
  maxConcurrentSteps: Int
)

object ConcurrentProgramGenerationSettings {
  val default: ConcurrentProgramGenerationSettings = ConcurrentProgramGenerationSettings(2, 1000, 2, 5)
}
