package com.pk.database;

public class MapHistory {
  private int gameId;
  private int step;
  private String stepBefore;
  private String stepAfter;

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

  public String getStepBefore() {
    return stepBefore;
  }

  public void setStepBefore(String stepBefore) {
    this.stepBefore = stepBefore;
  }

  public String getStepAfter() {
    return stepAfter;
  }

  public void setStepAfter(String stepAfter) {
    this.stepAfter = stepAfter;
  }

  public MapHistory() {
  }

  public MapHistory(int gameId, int step, String stepBefore, String stepAfter) {
    this.gameId = gameId;
    this.step = step;
    this.stepBefore = stepBefore;
    this.stepAfter = stepAfter;
  }

  public String toString() {
    return "[" + gameId + "] - " + step + " - " + stepBefore + " - " + stepAfter;
  }
}
