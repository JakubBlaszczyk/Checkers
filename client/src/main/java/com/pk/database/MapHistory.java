package com.pk.database;

public class MapHistory {
  private int gameId;
  private int step;
  private int stepBefore;
  private int stepAfter;

  public int getGameId() {
    return gameId;
  }

  public void setGameId(int gameId) {
    this.gameId = gameId;
  }

  public int getStep() {
    return step;
  }

  public void setStep(int step) {
    this.step = step;
  }

  public int getStepBefore() {
    return stepBefore;
  }

  public void setStepBefore(int stepBefore) {
    this.stepBefore = stepBefore;
  }

  public int getStepAfter() {
    return stepAfter;
  }

  public void setStepAfter(int stepAfter) {
    this.stepAfter = stepAfter;
  }

  public MapHistory() {
  }

  public MapHistory(int gameId, int step, int stepBefore, int stepAfter) {
    this.gameId = gameId;
    this.step = step;
    this.stepBefore = stepBefore;
    this.stepAfter = stepAfter;
  }

  public String toString() {
    return "[" + gameId + "] - " + step + " - " + stepBefore + " - " + stepAfter;
  }
}
